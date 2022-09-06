{-# LANGUAGE TemplateHaskell #-}

module Typer where

import Common
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (any, find)
import qualified Data.Map.Lazy as Map
import Data.Maybe
import qualified Data.Partition as P
import qualified Data.Set as Set
import Debug.Trace
import Native
import Parser

type LastVar = TypeVar

type TypeSets = P.Partition Type

type Bindings = Map.Map Id TypeVar

data TypeState = TypeState
  { _lastVar :: LastVar,
    _typeSets :: TypeSets,
    _bindings :: Bindings,
    _dataDefs :: [PData]
  }
  deriving (Show)

data Error
  = MatchError (Type, Type)
  | UndefinedVariableError Id
  | MultipleDefinitions Id
  | UndefinedConstructorError Id
  | BadConstructorPatternArity Id Int Int
  | UnresolvedVariable TypeVar PExpr
  | MainMissing
  | MainNotNullary
  | MissingClause PExpr
  deriving (Show)

type TyperMonad a = ExceptT Error (State TypeState) a

makeLenses ''TypeState

putType :: PExpr -> Type -> PExpr
putType e t = updateAnns (const $ Just t) e

getType :: PExpr -> Type
getType = fromJust . getAnns

typeExpr :: PExpr -> TyperMonad PExpr
typeExpr e@(PNum _ _) = return $ putType e $ TBase TInt
typeExpr (PApp as l r) = do
  l' <- typeExpr l
  let a = getType l'
  r' <- typeExpr r
  let b = getType r'
  c <- TVar <$> lift newVar
  unify a (TFun b c)
  return $ putType (PApp as l' r') c
typeExpr e@(PVar _ v) = do
  binds <- use bindings
  case Map.lookup v binds of
    Just tv -> return $ putType e $ TVar tv
    Nothing -> throwError $ UndefinedVariableError v
typeExpr (PAbs as x body) = do
  tv <- lift newVar
  body' <- evalInNewScope [(x, tv)] $ typeExpr body
  let bodyType = getType body'
  return $ putType (PAbs as x body') $ TFun (TVar tv) bodyType
typeExpr e@(PCase as x cls) = do
  x' <- typeExpr x
  let matchType = getType x'
  resType <- TVar <$> lift newVar
  cls' <- mapM (processClause matchType resType) cls
  checkClausesCoverage matchType cls
  return $ putType (PCase as x' cls') resType
  where
    checkClausesCoverage :: Type -> [PClause] -> TyperMonad ()
    checkClausesCoverage matchType cls = do
      ts <- use typeSets
      let (TData tName) = P.rep ts matchType
      dataTypes <- use dataDefs
      let (Just (PData _ constrs)) = find (\(PData n _) -> n == tName) dataTypes
      unless (all constrCovered constrs) $ throwError $ MissingClause e
      where
        constrCovered (PConstr cName _) = any (\(PClause (PPConstr n _) _) -> n == cName) cls
    processClause :: Type -> Type -> PClause -> TyperMonad PClause
    processClause matchT resT (PClause p@(PPVar v) body) = do
      tv <- lift newVar
      unify matchT (TVar tv)
      body' <- evalInNewScope [(v, tv)] $ typeExpr body
      unify resT (getType body')
      return $ PClause p body'
    processClause matchT resT (PClause p@(PPConstr c vars) body) = do
      binds <- use bindings
      case Map.lookup c binds of
        Nothing -> throwError $ UndefinedConstructorError c
        Just constrTV -> do
          ts <- use typeSets
          let constrType = P.rep ts (TVar constrTV)
          let constrFinalType = getConstrFinalType constrType
          unify matchT constrFinalType
          let constrArity = countArgs constrType
          let nVars = length vars
          when (constrArity /= nVars) $
            throwError $
              BadConstructorPatternArity c constrArity nVars
          varsTVs <- mapM (\_ -> lift newVar) vars
          mapM_ (uncurry unify) (zip (map TVar varsTVs) (getConstrArgTypes constrType))
          body' <- evalInNewScope (zip vars varsTVs) $ typeExpr body
          unify resT (getType body')
          return $ PClause p body'
      where
        countArgs (TFun _ x) = countArgs x + 1
        countArgs _ = 0
        getConstrFinalType (TFun _ x) = getConstrFinalType x
        getConstrFinalType x = x
        getConstrArgTypes (TFun x xs) = x : getConstrArgTypes xs
        getConstrArgTypes _ = []

newVar :: State TypeState TypeVar
newVar = do
  lastVar %= succ
  use lastVar

evalInNewScope :: [(Id, TypeVar)] -> TyperMonad a -> TyperMonad a
evalInNewScope vars f = do
  origBinds <- use bindings
  mapM_ (\(id, tv) -> bindings %= Map.insert id tv) vars
  res <- f
  bindings .= origBinds
  return res

unify :: Type -> Type -> TyperMonad ()
unify (TBase a) (TBase b)
  | a == b = return ()
  | otherwise = throwError $ MatchError (TBase a, TBase b)
unify (TFun a b) (TFun c d) = do
  unify a c
  unify b d
unify (TData a) (TData b)
  | a == b = return ()
  | otherwise = throwError $ MatchError (TData a, TData b)
unify a@(TVar _) b = do
  p <- use typeSets
  let a' = P.rep p a
  if a == a'
    then typeSets .= P.joinElems a b p
    else unify a' b
unify a b@(TVar _) = unify b a
unify a b = throwError $ MatchError (a, b)

runTypeExpr :: PExpr -> (Either Error PExpr, TypeState)
runTypeExpr expr = runState (runExceptT (loadNativeFuns >> typeExpr expr)) initState

initState = TypeState (TV "") P.empty Map.empty []

checkForDuplicateBind :: Id -> TyperMonad ()
checkForDuplicateBind id = do
  binds <- use bindings
  when (id `Map.member` binds) $ throwError $ MultipleDefinitions id

addTypedFun :: Id -> Type -> TyperMonad ()
addTypedFun id t = do
  checkForDuplicateBind id
  v <- lift newVar
  bindings %= Map.insert id v
  typeSets %= P.joinElems (TVar v) t

resolveTypeVars :: TypeSets -> PExpr -> TyperMonad PExpr
resolveTypeVars sets = postwalkM processNode
  where
    processNode :: PExpr -> TyperMonad PExpr
    processNode expr = putType expr <$> (pure (resolve $ getType expr) >>= check expr)
    resolve t@(TBase _) = t
    resolve t@(TData _) = t
    resolve (TFun a b) = TFun (resolve a) (resolve b)
    resolve t@(TVar _) = if rep == t then t else resolve rep
      where
        rep = P.rep sets t
    check :: PExpr -> Type -> TyperMonad Type
    check expr (TVar v) =
      throwError $ UnresolvedVariable v expr
    check expr x = pure x

loadNativeFuns :: TyperMonad ()
loadNativeFuns = mapM_ addNativeFun nativeFuns
  where
    addNativeFun (NativeFun name ty _) = addTypedFun name ty

typeDefs :: [PFun] -> TyperMonad [PFun]
typeDefs = mapM typeFun
  where
    typeFun :: PFun -> TyperMonad PFun
    typeFun (PFun fn body) = do
      body' <- typeExpr body
      let t = getType body'
      binds <- use bindings
      let Just var = Map.lookup fn binds
      unify (TVar var) t
      return (PFun fn body')

runTypeProg :: PProg -> (Either Error ([PFun], Type), TypeState)
runTypeProg prog = runState (runExceptT (prepare >> execute)) initState
  where
    prepare = loadNativeFuns >> addDefBinds >> addConstructors >> addDataDefs
    execute = typeDefs (funs prog) >>= resolveVars >>= attachMainType
    addDefBinds :: TyperMonad ()
    addDefBinds = mapM_ addFun $ funs prog
      where
        addFun :: PFun -> TyperMonad ()
        addFun (PFun fn _) = do
          checkForDuplicateBind fn
          v <- lift newVar
          bindings %= Map.insert fn v
    addConstructors :: TyperMonad ()
    addConstructors = mapM_ addData $ datas prog
      where
        addData :: PData -> TyperMonad ()
        addData (PData id constrs) = mapM_ (addConstr id) constrs
        addConstr :: Id -> PConstr -> TyperMonad ()
        addConstr typeId (PConstr constrId types) = do
          let t = foldr TFun (TData typeId) types
          addTypedFun constrId t
    addDataDefs :: TyperMonad ()
    addDataDefs = dataDefs .= datas prog
    resolveVars :: [PFun] -> TyperMonad [PFun]
    resolveVars fns = do
      ts <- use typeSets
      mapM (\(PFun id expr) -> PFun id <$> resolveTypeVars ts expr) fns
    attachMainType :: [PFun] -> TyperMonad ([PFun], Type)
    attachMainType funs = case mainType of
      Nothing -> throwError MainMissing
      (Just (TFun _ _)) -> throwError MainNotNullary
      (Just ty) -> pure (funs, ty)
      where
        mainFun = find (\(PFun id _) -> id == "main") funs
        mainType = fmap (\(PFun _ expr) -> getType expr) mainFun

-- return $ map (\(PFun id expr) -> PFun id (resolveTypeVars ts expr)) fns

printTypedExpr :: PExpr -> TypeSets -> String
printTypedExpr e sets = process e 0
  where
    process :: PExpr -> Int -> String
    process e@(PNum _ n) i = ind i (show n ++ " :: " ++ showType e)
    process e@(PVar _ x) i = ind i (x ++ " :: " ++ showType e)
    process e@(PApp _ l r) i =
      ind i ("( :: " ++ showType e)
        ++ "\n"
        ++ process l (i + 1)
        ++ "\n"
        ++ process r (i + 1)
        ++ "\n"
        ++ ind i ")"
    process e@(PAbs _ x b) i =
      ind i ("( :: " ++ showType e)
        ++ "\n"
        ++ ind (i + 1) ("\\" ++ x ++ " ->")
        ++ "\n"
        ++ process b (i + 1)
        ++ "\n"
        ++ ind i ")"
    process e@(PCase _ x cls) i =
      ind i ("( :: " ++ showType e)
        ++ "\n"
        ++ ind (i + 1) "case"
        ++ "\n"
        ++ process x (i + 2)
        ++ "\n"
        ++ ind (i + 1) "do"
        ++ "\n"
        ++ unlines (map printClause cls)
        ++ "\n"
        ++ ind (i + 1) "end"
        ++ "\n"
        ++ ind i ")"
      where
        printClause (PClause pattern body) =
          ind (i + 2) (printPattern pattern ++ " -> " ++ "\n" ++ process body (i + 3))
        printPattern (PPVar x) = x
        printPattern (PPConstr c args) = unwords (c : args)

    ind :: Int -> String -> String
    ind i s = concat (replicate i "  ") ++ s

    showType = show . getType
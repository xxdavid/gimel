{-# LANGUAGE TemplateHaskell #-}

module Typer where

import Common
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Lazy as Map
import Data.Maybe
import qualified Data.Partition as P
import qualified Data.Set as Set
import Debug.Trace
import Parser

type LastVar = TypeVar

type TypeSets = P.Partition Type

type Bindings = Map.Map Id TypeVar

data TypeState = TypeState {_lastVar :: LastVar, _typeSets :: TypeSets, _bindings :: Bindings}
  deriving (Show)

data Error = MatchError (Type, Type) | UndefinedVariableError Id | MultipleDefinitions Id
  deriving (Show)

type TyperMonad a = ExceptT Error (State TypeState) a

makeLenses ''TypeState

addType :: PExpr -> Type -> PExpr
addType e t = addAnn (AType t) e

getType :: PExpr -> Type
getType = unpack . getAnn AKType
  where
    unpack (Just (AType t)) = t
    unpack _ = error "Invalid annotation"

typeExpr :: PExpr -> TyperMonad PExpr
typeExpr e@(PNum _ _) = return $ addType e $ TBase TInt
typeExpr (PApp as l r) = do
  l' <- typeExpr l
  let a = getType l'
  r' <- typeExpr r
  let b = getType r'
  c <- TVar <$> lift newVar
  unify a (TFun b c)
  return $ addType (PApp as l' r') c
typeExpr e@(PVar _ v) = do
  binds <- use bindings
  case Map.lookup v binds of
    Just tv -> return $ addType e $ TVar tv
    Nothing -> throwError $ UndefinedVariableError v
typeExpr (PAbs as x body) = do
  origBinds <- use bindings
  tv <- lift newVar
  bindings %= Map.insert x tv
  body' <- typeExpr body
  let bodyType = getType body'
  bindings .= origBinds
  return $ addType (PAbs as x body') $ TFun (TVar tv) bodyType

newVar :: State TypeState TypeVar
newVar = do
  lastVar %= succ
  use lastVar

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
runTypeExpr expr = runState (runExceptT (loadPredefined >> typeExpr expr)) initState

initState = TypeState (TV "") P.empty Map.empty

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

loadPredefined :: TyperMonad ()
loadPredefined = mapM_ (uncurry addTypedFun) predefinedTypes

predefinedTypes :: [(Id, Type)]
predefinedTypes =
  [ ("+", TFun (TBase TInt) (TFun (TBase TInt) (TBase TInt))),
    ("-", TFun (TBase TInt) (TFun (TBase TInt) (TBase TInt))),
    ("*", TFun (TBase TInt) (TFun (TBase TInt) (TBase TInt))),
    ("/", TFun (TBase TInt) (TFun (TBase TInt) (TBase TInt))),
    ("succ", TFun (TBase TInt) (TBase TInt))
  ]

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

runTypeProg :: PProg -> (Either Error [PFun], TypeState)
runTypeProg prog = runState (runExceptT (prepare >> typeDefs (funs prog))) initState
  where
    prepare = loadPredefined >> addDefBinds >> addConstructors
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

    ind :: Int -> String -> String
    ind i s = concat (replicate i "  ") ++ s

    showType = show . replaceVars . getType
    replaceVars :: Type -> Type
    replaceVars t@(TBase _) = t
    replaceVars t@(TData _) = t
    replaceVars (TFun a b) = TFun (replaceVars a) (replaceVars b)
    replaceVars t@(TVar _) = P.rep sets t
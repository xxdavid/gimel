{-# LANGUAGE TemplateHaskell #-}

module Typer where

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
import Types

type LastVar = TypeVar

type TypeSets = P.Partition Type

type Bindings = Map.Map Id TypeVar

data TypeState = TypeState {_lastVar :: LastVar, _typeSets :: TypeSets, _bindings :: Bindings}
  deriving (Show)

data Error = MatchError (Type, Type) | UndefinedVariableError Id
  deriving (Show)

makeLenses ''TypeState

addType :: PExpr -> Type -> PExpr
addType e t = addAnn (AType t) e

getType :: PExpr -> Type
getType = unpack . getAnn AKType
  where
    unpack (Just (AType t)) = t
    unpack _ = error "Invalid annotation"

typeExpr :: PExpr -> ExceptT Error (State TypeState) PExpr
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

unify :: Type -> Type -> ExceptT Error (State TypeState) ()
unify (TBase a) (TBase b)
  | a == b = return ()
  | otherwise = throwError $ MatchError (TBase a, TBase b)
unify (TFun a b) (TFun c d) = do
  unify a c
  unify b d
unify a@(TVar _) b = do
  p <- use typeSets
  let a' = P.rep p a
  if a == a'
    then typeSets .= P.joinElems a b p
    else unify a' b
unify a b@(TVar _) = unify b a
unify a b = throwError $ MatchError (a, b)

runTypeExpr :: PExpr -> (Either Error PExpr, TypeState)
runTypeExpr expr = runState (loadPredefined >> runExceptT (typeExpr expr)) initState

initState = TypeState (TV "") P.empty Map.empty

loadPredefined :: State TypeState ()
loadPredefined = mapM_ addFun predefinedTypes
  where
    addFun :: (Id, Type) -> State TypeState ()
    addFun (f, t) = do
      v <- newVar
      bindings %= Map.insert f v
      typeSets %= P.joinElems (TVar v) t

predefinedTypes :: [(Id, Type)]
predefinedTypes =
  [ ("+", TFun (TBase TInt) (TFun (TBase TInt) (TBase TInt))),
    ("-", TFun (TBase TInt) (TFun (TBase TInt) (TBase TInt))),
    ("*", TFun (TBase TInt) (TFun (TBase TInt) (TBase TInt))),
    ("/", TFun (TBase TInt) (TFun (TBase TInt) (TBase TInt))),
    ("succ", TFun (TBase TInt) (TBase TInt))
  ]

typeDefs :: [PDef] -> ExceptT Error (State TypeState) [PDef]
typeDefs = mapM typeFun
  where
    typeFun :: PDef -> ExceptT Error (State TypeState) PDef
    typeFun (PFun fn body) = do
      body' <- typeExpr body
      let t = getType body'
      binds <- use bindings
      let Just var = Map.lookup fn binds
      unify (TVar var) t
      return (PFun fn body')

runTypeDefs :: [PDef] -> (Either Error [PDef], TypeState)
runTypeDefs defs = runState (loadPredefined >> addDefBinds >> runExceptT (typeDefs defs)) initState
  where
    addDefBinds :: State TypeState ()
    addDefBinds = mapM_ addFun defs
      where
        addFun :: PDef -> State TypeState ()
        addFun (PFun fn _) = do
          v <- newVar
          bindings %= Map.insert fn v

printTypedExpr :: PExpr -> String
printTypedExpr e = process e 0
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

    showType = show . getType
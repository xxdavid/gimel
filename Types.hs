{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Lazy as Map
import qualified Data.Partition as P
import qualified Data.Set as Set
import Debug.Trace
import Parser

data BaseType = TInt
  deriving (Show, Eq, Ord)

data Type
  = TBase BaseType
  | TFun Type Type
  | TVar TypeVar
  deriving (Eq, Ord)

newtype TypeVar = TV {unTV :: String}
  deriving (Eq)

instance Show TypeVar where
  show (TV v) = v

instance Show Type where
  show (TBase TInt) = "Int"
  show (TVar tv) = show tv
  show (TFun a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"

instance Enum TypeVar where
  fromEnum (TV v) = fromEnumTV v
    where
      charPos :: Char -> Int
      charPos x = fromEnum x - fromEnum 'a' + 1

      fromEnumTVAux :: String -> Int
      fromEnumTVAux [] = 0
      fromEnumTVAux (x : xs) = charPos x + 26 * fromEnumTVAux xs

      fromEnumTV :: String -> Int
      fromEnumTV v = fromEnumTVAux (reverse v) - 1

  toEnum = TV . toEnumTV
    where
      charAt :: Int -> Char
      charAt i = toEnum (i + fromEnum 'a')

      toEnumTVAux :: Int -> String
      toEnumTVAux 0 = []
      toEnumTVAux i = charAt rem : toEnumTVAux ((i - rem) `div` 26)
        where
          rem = (i - 1) `mod` 26

      toEnumTV :: Int -> String
      toEnumTV = reverse . toEnumTVAux . (+ 1)

instance Ord TypeVar where
  a <= b = fromEnum a <= fromEnum b

type LastVar = TypeVar

type TypeEnv = Map.Map Id Type

type TypeSets = P.Partition Type

data TypeState = TypeState {_lastVar :: LastVar, _typeSets :: TypeSets} deriving (Show)

type Error = (Type, Type)

makeLenses ''TypeState

typeExpr :: PExpr -> ExceptT Error (State TypeState) Type
typeExpr (PNum _) = return $ TBase TInt
typeExpr (PApp l r) = do
  a <- typeExpr l
  b <- typeExpr r
  c <- TVar <$> lift newVar
  unify a (TFun b c)
  return c
typeExpr (PVar v) = TVar <$> lift newVar

newVar :: State TypeState TypeVar
newVar = do
  lastVar %= succ
  gets _lastVar

unify :: Type -> Type -> ExceptT Error (State TypeState) ()
unify (TBase a) (TBase b)
  | a == b = return ()
  | otherwise = throwError (TBase a, TBase b)
unify (TFun a b) (TFun c d) = do
  unify a c
  unify b d
unify a@(TVar _) b = do
  p <- gets _typeSets
  let a' = P.rep p a
  if a == a'
    then typeSets .= P.joinElems a b p
    else unify a' b
unify a b@(TVar _) = unify b a
unify a b = throwError (a, b)

runTypeExpr :: PExpr -> (Either Error Type, TypeState)
runTypeExpr expr = runState (runExceptT $ typeExpr expr) initState
  where
    initState = TypeState (TV "") P.empty

-- predefinedTypes :: [(Id, Type)]
-- predefinedTypes = [
-- ("+", TFun (TBase TInt) (TFun (TBase TInt) (TBase TInt))),
-- ("-", TFun (TBase TInt) (TFun (TBase TInt) (TBase TInt))),
-- ("*", TFun (TBase TInt) (TFun (TBase TInt) (TBase TInt))),
-- ("/", TFun (TBase TInt) (TFun (TBase TInt) (TBase TInt)))
-- ]

-- initialTypeSets = P.fromSets $ map (\(f, t) -> Set.fromList [TVar (TV f), t]) predefinedTypes

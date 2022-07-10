module Types where

import Parser
import Control.Monad.State
import qualified Data.Map.Lazy as Map
import qualified Data.Partition as P
import Debug.Trace

data BaseType = TInt
    deriving (Show, Eq, Ord)

data Type
    = TBase BaseType
    | TFun Type Type
    | TVar TypeVar
    deriving (Eq, Ord)

newtype TypeVar = TV { unTV :: String }
    deriving Eq

instance Show TypeVar where
    show (TV v) = v

instance Show Type where
    show (TBase TInt) = "Int"
    show (TVar tv) = show tv
    show (TFun a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"

instance Enum TypeVar where
    fromEnum (TV v) = fromEnumTV v where
        charPos :: Char -> Int
        charPos x = fromEnum x - fromEnum 'a'  + 1

        fromEnumTVAux :: String -> Int
        fromEnumTVAux [] = 0
        fromEnumTVAux (x:xs) = charPos x + 26 * fromEnumTVAux xs

        fromEnumTV :: String -> Int
        fromEnumTV v = fromEnumTVAux (reverse v) - 1

    toEnum = TV . toEnumTV where
        charAt :: Int -> Char
        charAt i = toEnum (i + fromEnum 'a')

        toEnumTVAux :: Int -> String
        toEnumTVAux 0 = []
        toEnumTVAux i = charAt rem : toEnumTVAux ((i - rem) `div` 26)
            where rem = (i - 1) `mod` 26


        toEnumTV :: Int -> String
        toEnumTV = reverse . toEnumTVAux . (+1)

instance Ord TypeVar where
    a <= b = fromEnum a <= fromEnum b

type LastVar = TypeVar
type TypeEnv = Map.Map Id Type
type TypeState = (LastVar, TypeEnv)

type TypeSets = P.Partition Type
type TypeState' = (LastVar, TypeSets)

typeExpr :: PExpr -> TypeState -> (Type, TypeState)
typeExpr (PNum _) (lv, env) = (TBase TInt, (lv, env))
typeExpr (PApp l r) (lv, env) = (TFun a b, (lv'', env'')) where
    (a, (lv', env')) = typeExpr l (lv, env)
    (b, (lv'', env'')) = typeExpr r (lv', env')
typeExpr (PVar v) (lv, env) = (TVar a, (a, env')) where
    a = succ lv
    env' = Map.insert v (TVar a) env

typeExpr' :: PExpr -> State TypeState' Type
typeExpr' (PNum _) = return $ TBase TInt
typeExpr' (PApp l r) = do
    a <- typeExpr' l
    b <- typeExpr' r
    s <- get
    c <- TVar <$> newVar
    s' <- get
    let p' = unify a (TFun b c) (snd s)
    let s'' = (fst s', p') 
    put s''
    return c
typeExpr' (PVar v) = do TVar <$> newVar

newVar :: State TypeState' TypeVar
newVar = do
    s <- get
    let lv = fst s
    let a = succ lv
    let s' = (a, snd s)
    put s'
    return a

runTypeExpr :: PExpr -> Type
runTypeExpr e = fst $ typeExpr e (TV "", mempty)

exploreTypeExpr :: PExpr -> (Type, TypeState)
exploreTypeExpr e = typeExpr e (TV "", mempty)


-- TODO: use Either (Type, Type) (P.Partition Type)
unify :: Type -> Type -> P.Partition Type -> P.Partition Type
unify (TBase a) (TBase b) p
  | a == b = p
  | otherwise = error "A"
unify (TFun a b) (TFun c d) p = do
    let p' = unify a c p
    unify b d p'
unify a@(TVar _) b p = do
    let a' = P.rep p a
    if a == a' then P.joinElems a b p
               else unify a' b p
unify a b@(TVar _) p = unify b a p
unify _ _ _ = error "B"



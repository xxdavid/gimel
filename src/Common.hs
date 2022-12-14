module Common where

import Data.Functor.Identity

type Id = String

-- * Abstract Syntax Tree

data PFun = PFun Id PExpr
  deriving (Eq, Show)

data PData = PData Id [PConstr]
  deriving (Eq, Show)

data PProg = PProg {funs :: [PFun], datas :: [PData]}
  deriving (Eq, Show)

data PConstr = PConstr Id [Type] deriving (Eq, Show)

data PClause = PClause PPattern PExpr deriving (Eq)

instance Show PClause where
  show (PClause pattern expr) = show pattern ++ " -> " ++ show expr

data PPattern
  = PPVar Id
  | PPConstr Id [Id]
  deriving (Eq)

instance Show PPattern where
  show (PPVar v) = v
  show (PPConstr n ps) = n ++ unwords ps

data PExpr
  = PNum Ann Int
  | PApp Ann PExpr PExpr
  | PVar Ann Id
  | PAbs Ann Id PExpr
  | PCase Ann PExpr [PClause]
  deriving (Eq)

instance Show PExpr where
  show (PNum _ n) = show n
  show (PApp _ l r) = "(" ++ show l ++ " " ++ show r ++ ")"
  show (PVar _ v) = v
  show (PAbs _ x e) = "(\\" ++ x ++ " -> " ++ show e ++ ")"
  show (PCase _ m cls) = "case " ++ show m ++ " do\n" ++ unlines (map (('\t' :) . show) cls) ++ "end"

postwalk :: (PExpr -> PExpr) -> PExpr -> PExpr
postwalk f = runIdentity . postwalkM ((pure . f) :: (PExpr -> Identity PExpr))

postwalkM :: Monad m => (PExpr -> m PExpr) -> PExpr -> m PExpr
postwalkM f e@(PNum _ _) = f e
postwalkM f (PApp as a b) = do
  a' <- postwalkM f a
  b' <- postwalkM f b
  f $ PApp as a' b'
postwalkM f e@(PVar _ _) = f e
postwalkM f (PAbs as v b) = do
  b' <- postwalkM f b
  f $ PAbs as v b'
postwalkM f (PCase as x cls) = do
  x' <- postwalkM f x
  cls' <- mapM walkClause cls
  f $ PCase as x' cls'
  where
    walkClause (PClause ptn e) = do
      e' <- postwalkM f e
      return $ PClause ptn e'

lambdasToParams :: PExpr -> ([Id], PExpr)
lambdasToParams (PAbs _ x body) = (x : nextParams, body')
  where
    (nextParams, body') = lambdasToParams body
lambdasToParams e = ([], e)

-- * Types

data BaseType = TInt
  deriving (Show, Eq, Ord)

data Type
  = TBase BaseType
  | TData Id
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
  show (TData t) = t

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

-- * Annotations

type Ann = Maybe Type

emptyAnn :: Ann
emptyAnn = Nothing

updateAnns :: (Ann -> Ann) -> PExpr -> PExpr
updateAnns f (PNum as a) = PNum (f as) a
updateAnns f (PApp as a b) = PApp (f as) a b
updateAnns f (PVar as a) = PVar (f as) a
updateAnns f (PAbs as a b) = PAbs (f as) a b
updateAnns f (PCase as a b) = PCase (f as) a b

getAnns :: PExpr -> Ann
getAnns (PNum as _) = as
getAnns (PApp as _ _) = as
getAnns (PVar as _) = as
getAnns (PAbs as _ _) = as
getAnns (PCase as _ _) = as
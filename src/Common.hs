module Common where

import Data.Foldable (find)

type Id = String

-- * Abstract Syntax Tree

data PFun = PFun Id PExpr
  deriving (Eq, Show)

data PData = PData Id [PConstr]
  deriving (Eq, Show)

data PDef = PDFun PFun | PDData PData
  deriving (Eq, Show)

data PProg = PProg {funs :: [PFun], datas :: [PData]}
  deriving (Eq, Show)

data PConstr = PConstr Id [Type] deriving (Eq, Show)

data PExpr
  = PNum Ann Int
  | PApp Ann PExpr PExpr
  | PVar Ann Id
  | PAbs Ann Id PExpr
  deriving (Eq, Show)

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

data Annotation = AType Type deriving (Eq, Show)

data AnnKind = AKType

type Ann = [Annotation]

addAnn :: Annotation -> PExpr -> PExpr
addAnn x (PNum xs a) = PNum (x : xs) a
addAnn x (PApp xs a b) = PApp (x : xs) a b
addAnn x (PVar xs a) = PVar (x : xs) a
addAnn x (PAbs xs a b) = PAbs (x : xs) a b

getAnns :: PExpr -> Ann
getAnns (PNum xs _) = xs
getAnns (PApp xs _ _) = xs
getAnns (PVar xs _) = xs
getAnns (PAbs xs _ _) = xs

matchAnnKind :: AnnKind -> Annotation -> Bool
matchAnnKind AKType (AType _) = True

getAnn :: AnnKind -> PExpr -> Maybe Annotation
getAnn kind e = find (matchAnnKind kind) $ getAnns e

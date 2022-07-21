module Types where

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
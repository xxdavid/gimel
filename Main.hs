module Main where

import Lexer
import Parser
import Typer

deleteLast [h] = []
deleteLast (x : xs) = x : deleteLast xs

main = do
  s <- getContents
  let tokens = alexScanTokens (deleteLast s)
  print tokens
  let ast = parse tokens
  print ast
  let typeRes = runTypeDefs ast
  print typeRes
  case typeRes of
    (Right t, _) -> mapM_ printDef t
    _ -> pure ()
  where
    printDef (PFun fn body) = putStrLn $ fn ++ " = " ++ printTypedExpr body

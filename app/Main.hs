module Main where

import Common
import Control.Lens.Getter ((^.))
import Lexer
import Parser
import Typer

deleteLast [h] = []
deleteLast (x : xs) = x : deleteLast xs

main = do
  s <- getContents
  let tokens = alexScanTokens (deleteLast s)
  print tokens
  let prog = parse tokens
  print prog
  let typeRes = runTypeProg prog
  print typeRes
  case typeRes of
    (Right t, state) -> mapM_ (printDef $ state ^. typeSets) t
    _ -> pure ()
  where
    printDef sets (PFun fn body) = putStrLn $ fn ++ " = " ++ printTypedExpr body sets

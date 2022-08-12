module Main where

import Common
import Control.Lens.Getter ((^.))
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import GMachine
import Lexer
import Parser
import Typer

main = do
  s <- getContents
  let tokens = alexScanTokens (trim s)
  print tokens
  let prog = parse tokens
  print prog
  let typeRes = runTypeProg prog
  print typeRes
  case typeRes of
    (Right typedFuns, state) -> do
      mapM_ (printDef $ state ^. typeSets) typedFuns
      let instrs = compileProg prog
      print instrs
    _ -> pure ()
  where
    printDef sets (PFun fn body) = putStrLn $ fn ++ " = " ++ printTypedExpr body sets
    trim = dropWhileEnd isSpace . dropWhile isSpace

constructExpr :: String -> PExpr
constructExpr = parseExpr . alexScanTokens

processExpr :: String -> PExpr
processExpr = unpack . runTypeExpr . constructExpr
  where
    unpack (Right x, _) = x
    unpack (Left err, _) = error $ show err

module Main where

import Codegen
import Common
import Control.Lens.Getter ((^.))
import Data.Char (isSpace)
import GMachine
import Lexer
import Parser
import Typer

main = do
  s <- getContents
  let tokens = tokenize s
  -- print tokens
  let prog = parse tokens
  -- print prog
  let typeRes = runTypeProg prog
  -- print typeRes
  case typeRes of
    (Right typedFuns, state) -> do
      -- mapM_ (printDef $ state ^. typeSets) typedFuns
      let compiledFuns = compileProg prog
      compileToBinary prog compiledFuns "program"
      -- print compiledFuns
      pure ()
    other -> print other
  where
    printDef sets (PFun fn body) = putStrLn $ fn ++ " = " ++ printTypedExpr body sets

constructExpr :: String -> PExpr
constructExpr = parseExpr . tokenize

processExpr :: String -> PExpr
processExpr = unpack . runTypeExpr . constructExpr
  where
    unpack (Right x, _) = x
    unpack (Left err, _) = error $ show err

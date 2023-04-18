{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Codegen
import Common
import Config
import Control.Lens.Getter ((^.))
import Control.Monad (when)
import Data.Char (isSpace)
import GMachine
import Lexer
import Parser
import Stdlib
import System.Console.CmdArgs
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)
import Typer

config =
  Config
    { src = def &= typ "FILE" &= argPos 0,
      out = def &= help "Output file" &= typFile,
      run = False &= help "Run program after compilation",
      verbose = False &= help "Verbose output"
    }

getConfig =
  cmdArgs $
    config
      &= help "Compile programs in the Gimel programming language"
      &= program "gimel"
      &= summary "Gimel compiler v1.0"

main = do
  config <- getConfig
  s <- readFile $ src config
  stdlib <- readStdlib
  let programText = s ++ stdlib
  let tokens = tokenize programText
  when (verbose config) $ print tokens
  let prog = parse tokens
  let typeRes = runTypeProg prog
  case typeRes of
    (Right (typedFuns, mainType), state) -> do
      when (verbose config) $ mapM_ (printDef $ state ^. typeSets) typedFuns
      let compiledFuns = compileProg prog
      when (verbose config) $ print compiledFuns
      compileToBinary prog compiledFuns mainType config
    (Left error, _) -> do
      hPutStrLn stderr $ errorMsg error
      exitWith (ExitFailure 1)
  where
    printDef sets (PFun fn body) = putStrLn $ fn ++ " = " ++ printTypedExpr body sets

errorMsg :: Error -> String
errorMsg (MatchError a b) = unwords ["Cannot match type", show a, "with type", show b] ++ "."
errorMsg (UndefinedVariableError name) = unwords ["Undefined variable", name] ++ "."
errorMsg (MultipleDefinitions name) = unwords ["Multiple definitions of function", name] ++ "."
errorMsg (UndefinedConstructorError name) = unwords ["Matching against an undefined constructor", name] ++ "."
errorMsg (BadConstructorPatternArity name expected actual) =
  unwords
    [ "Bad",
      name,
      "constructor arity, expected",
      show expected,
      "arguments, got",
      show actual,
      "arguments"
    ]
    ++ "."
errorMsg (UnresolvedVariable _ expr) =
  unwords
    ["Unresolved type for expression", show expr, "(it is probably too general)"]
    ++ "."
errorMsg MainMissing = "The main function is missing."
errorMsg MainNotNullary = "The main function takes arguments but it should take none."
errorMsg (MissingClause expr) = "The following case expression does not handle all possible cases: \n" ++ show expr

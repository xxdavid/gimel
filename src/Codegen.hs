{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Codegen where

import Common
import Config
import Control.Monad (void, when)
import Control.Monad.Fix (MonadFix)
import qualified Data.Map as Map
import Data.String (fromString)
import qualified Data.Text.Lazy.IO as Text
import Debug.Trace (trace)
import GHC.IO (bracket)
import GMachine (CompiledFun (CompiledFun), Instr (..))
import LLVM.AST hiding (function)
import LLVM.AST.Constant (Constant (GlobalReference))
import LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Type (Type (VoidType), i32, ptr)
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.Pretty (ppllvm)
import LlvmCommon
import Native
import Paths_gimel
import qualified Runtime as R
import System.Directory (makeAbsolute, removePathForcibly, withCurrentDirectory)
import System.FilePath (dropExtensions)
import System.IO (hClose)
import System.Posix.Temp (mkdtemp, mkstemp, mkstemps)
import System.Process (callProcess, readProcess)

call' fun args = call fun (map (,[]) args)

callRt decl = call' (declToOperand decl)

int32' n = int32 (toInteger n)

type FunOperands = Map.Map Id Operand

type FunArities = Map.Map Id Int

type FunBuilder = IRBuilderT ModuleBuilder

type Stack = Operand

funSymbolName :: Id -> Name
funSymbolName name = fromString $ "f_" ++ name

buildFunArities :: PProg -> FunArities
buildFunArities prog = funArities `Map.union` dataArities `Map.union` nativeArities
  where
    funArities = foldr insertFun Map.empty (funs prog)
    insertFun (PFun name expr) = Map.insert name arity
      where
        (params, _) = lambdasToParams expr
        arity = length params
    dataArities = foldr insertData Map.empty (datas prog)
    insertData (PData _ constrs) map = foldr insertConstr map constrs
    insertConstr (PConstr name params) = Map.insert name (length params)
    nativeArities = foldr insertNative Map.empty nativeFuns
    insertNative (NativeFun name ty _) = Map.insert name (countArgs ty)
    countArgs (TFun _ next) = 1 + countArgs next
    countArgs _ = 0

codegenInstr :: Stack -> FunArities -> Instr -> FunBuilder Stack
codegenInstr s _ (PushInt n) = callRt R.pushInt [s, int32 (toInteger n)]
codegenInstr s _ MkApp = callRt R.mkApp [s]
codegenInstr s fa (PushGlobal name) = callRt R.pushGlobal [s, arityOp, operand]
  where
    operand = funOperand (funSymbolName name) [R.stackT] R.stackT
    arity = Map.findWithDefault (error $ "Unknown arity of " ++ name) name fa
    arityOp = int32 (toInteger arity)
codegenInstr s _ (Update n) = callRt R.update [s, int32' n]
codegenInstr s _ (Pop n) = callRt R.pop [s, int32' n]
codegenInstr s _ (Slide n) = callRt R.slide [s, int32' n]
codegenInstr s _ (Push n) = callRt R.pushInstr [s, int32' n]
codegenInstr s _ (Pack tag n) = callRt R.pack [s, int32' tag, int32' n]
codegenInstr s _ (Split n) = callRt R.split [s, int32' n]
codegenInstr s _ Eval = callRt R.eval [s]
codegenInstr s fa (Jump branches) = mdo
  node <- callRt R.peek [s]
  dTag <- callRt R.getConstrTag [node]

  opts <- mapM (codegenBranch s fa dTag end) branches

  -- the last next block is unreachable (one of branches must match)
  unreachable

  end <- block
  sFin <- phi opts
  pure sFin

codegenBranch s fa dTag end (tag, instrs) = mdo
  select <- icmp IP.EQ dTag (int32' tag)
  condBr select body next

  body <- block
  finalBodyStack <- codegenInstrs s fa instrs
  -- generated instrs may change the block
  finalBodyBlock <- currentBlock
  br end

  next <- block
  pure (finalBodyStack, finalBodyBlock)

codegenInstrs :: Stack -> FunArities -> [Instr] -> FunBuilder Stack
codegenInstrs stack fa = foldl (\sm i -> sm >>= (\s -> codegenInstr s fa i)) (pure stack)

codegenNativeFun :: NativeFun -> ModuleBuilder Operand
codegenNativeFun (NativeFun name _ (ArithOp op)) = codegenNativeArithOp name op
codegenNativeFun (NativeFun name _ (CompOp pred)) = codegenNativeCompOp name pred

codegenNativeBinaryIntFunction name innerInstrs = do
  function (funSymbolName name) [(R.stackT, "stack")] R.stackT $ \[s] -> do
    entry <- block `named` "entry"

    l <- callRt R.lookup [s, int32 0]
    le <- callRt R.evalNode [l]
    lInt <- callRt R.getInt [le]

    r <- callRt R.lookup [s, int32 1]
    re <- callRt R.evalNode [r]
    rInt <- callRt R.getInt [re]

    s <- innerInstrs s lInt rInt

    s <- callRt R.update [s, int32 2]
    s <- callRt R.pop [s, int32 2]
    ret s

codegenNativeArithOp name op =
  codegenNativeBinaryIntFunction name $ \s lInt rInt -> do
    res <- op lInt rInt
    callRt R.pushInt [s, res]

codegenNativeCompOp name pred =
  codegenNativeBinaryIntFunction name $ \s lInt rInt -> mdo
    res <- icmp pred lInt rInt
    condBr res true false

    true <- block
    sTrue <- callRt R.pack [s, int32' 0, int32' 0]
    br end

    false <- block
    sFalse <- callRt R.pack [s, int32' 1, int32' 0]
    br end

    end <- block
    phi [(sTrue, true), (sFalse, false)]

codegenCompiledFun :: CompiledFun -> FunArities -> ModuleBuilder Operand
codegenCompiledFun (CompiledFun name instrs) fa = do
  function (funSymbolName name) [(R.stackT, "stack")] R.stackT $ \[s] -> do
    entry <- block `named` "entry"
    s <- codegenInstrs s fa instrs
    ret s

codegenPrintResult :: Stack -> Common.Type -> FunBuilder Operand
codegenPrintResult s (TBase baseType) = codegenPrintBase s baseType
codegenPrintResult s (TData name) = codegenPrintData s name
codegenPrintResult _ _ = error "Cannot print unknown type"

codegenPrintBase :: Stack -> BaseType -> FunBuilder Operand
codegenPrintBase s TInt = callRt R.printIntNode [s] >> callRt R.pop [s, int32 1]

codegenPrintData :: Stack -> Id -> FunBuilder Operand
codegenPrintData s name = call' (funOperand printerName [R.stackT] R.stackT) [s]
  where
    printerName = dataPrinterSymbolName name

dataPrinterSymbolName :: Id -> Name
dataPrinterSymbolName name = fromString $ "dprint_" ++ name

codegenDataPrinter :: PData -> ModuleBuilder Operand
codegenDataPrinter (PData tName constrs) = do
  function (dataPrinterSymbolName tName) [(R.stackT, "stack")] R.stackT $ \[s] -> mdo
    node <- callRt R.peek [s]
    nTag <- callRt R.getConstrTag [node]

    opts <- mapM (codegenDataPrinterBranch s nTag end) (zip [0 ..] constrs)

    -- the last next block is unreachable (one of branches must match)
    unreachable

    end <- block
    sFin <- phi opts
    ret sFin

codegenDataPrinterBranch s nTag end (cTag, PConstr cName params) = mdo
  let arity = length params
  let printParens = arity > 0

  select <- icmp IP.EQ nTag (int32' cTag)
  condBr select body next

  body <- block

  when printParens $ void $ callRt R.printLParen []
  str <- globalStringPtr cName (Name (fromString cName))
  callRt R.printStr [ConstantOperand str]

  s' <- callRt R.split [s, int32' arity]

  finalBodyStack <- foldl (\sm p -> sm >>= (\s -> codegenDataPrinterParam s p)) (pure s') params
  finalBodyBlock <- currentBlock
  when printParens $ void $ callRt R.printRParen []
  br end

  next <- block
  pure (finalBodyStack, finalBodyBlock)

codegenDataPrinterParam :: Stack -> Common.Type -> FunBuilder Stack
codegenDataPrinterParam s ty = do
  callRt R.printSpace []
  s <- callRt R.eval [s]
  codegenPrintResult s ty

codegenProgram :: PProg -> [CompiledFun] -> Common.Type -> Module
codegenProgram prog cFuns resType = buildModule "mainModule" $ do
  mapM_ (\(FunDecl name argtys retty) -> extern name argtys retty) R.all
  let fa = buildFunArities prog
  mapM_ (\cFun -> codegenCompiledFun cFun fa) cFuns
  mapM_ codegenNativeFun nativeFuns
  mapM_ codegenDataPrinter (datas prog)

  function "main" [] VoidType $ \[] -> do
    entry <- block `named` "entry"

    s <- callRt R.emptyStack []
    s <- codegenInstrs s fa [PushGlobal "main"]
    s <- callRt R.unwind [s]

    codegenPrintResult s resType
    callRt R.printNl []

    retVoid

-- taken from https://blog.josephmorag.com/posts/mcc3/
compileToBinary :: PProg -> [CompiledFun] -> Common.Type -> Config -> IO ()
compileToBinary prog cFuns resType config = do
  outfile <- case (out config, run config) of
    (Just path, _) -> makeAbsolute path
    (Nothing, False) -> makeAbsolute $ dropExtensions (src config)
    (Nothing, True) -> fst <$> mkstemp "gimel_program"

  bracket (mkdtemp "build") removePathForcibly $ \buildDir ->
    withCurrentDirectory buildDir $ do
      (llvm, llvmHandle) <- mkstemps "output" ".ll"
      runtime <- getDataFileName "assets/runtime.c"

      let llvmModule = codegenProgram prog cFuns resType
      let moduleText = ppllvm llvmModule
      when (verbose config) $ Text.putStrLn moduleText

      Text.hPutStrLn llvmHandle moduleText
      hClose llvmHandle

      callProcess "clang" ["-Wno-override-module", "-lm", llvm, runtime, "-o", outfile]

      when (run config) $ readProcess outfile [] [] >>= print
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Codegen where

import Common
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
import qualified Runtime as R
import System.Directory (removePathForcibly, withCurrentDirectory)
import System.IO (hClose)
import System.Posix.Temp (mkdtemp, mkstemps)
import System.Process (callProcess)

call' fun args = call fun (map (,[]) args)

callRt decl = call' (declToOperand decl)

int32' n = int32 (toInteger n)

type FunOperands = Map.Map Id Operand

type FunArities = Map.Map Id Int

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

codegenInstr :: (MonadIRBuilder m, MonadFix m) => Operand -> FunArities -> Instr -> m Operand
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

codegenInstrs :: (MonadIRBuilder m, MonadFix m) => Operand -> FunArities -> [Instr] -> m Operand
codegenInstrs stack fa = foldl (\sm i -> sm >>= (\s -> codegenInstr s fa i)) (pure stack)

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

codegenProgram :: PProg -> [CompiledFun] -> Module
codegenProgram prog cFuns = buildModule "mainModule" $ do
  mapM_ (\(FunDecl name argtys retty) -> extern name argtys retty) R.all
  let fa = buildFunArities prog
  mapM_ (\cFun -> codegenCompiledFun cFun fa) cFuns
  mapM_ codegenNativeFun nativeFuns

  function "main" [] VoidType $ \[] -> do
    entry <- block `named` "entry"

    s <- callRt R.emptyStack []
    s <- codegenInstrs s fa [PushGlobal "main"]
    s <- callRt R.unwind [s]
    callRt R.printStack [s]

    retVoid

-- taken from https://blog.josephmorag.com/posts/mcc3/
compileToBinary :: PProg -> [CompiledFun] -> FilePath -> IO ()
compileToBinary prog cFuns outfile =
  bracket (mkdtemp "build") removePathForcibly $ \buildDir ->
    withCurrentDirectory buildDir $ do
      (llvm, llvmHandle) <- mkstemps "output" ".ll"
      let runtime = "../assets/runtime.c"

      let llvmModule = codegenProgram prog cFuns
      let moduleText = ppllvm llvmModule
      Text.putStrLn moduleText

      Text.hPutStrLn llvmHandle moduleText
      hClose llvmHandle

      callProcess
        "clang"
        ["-Wno-override-module", "-lm", llvm, runtime, "-o", "../" <> outfile]
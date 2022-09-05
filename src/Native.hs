module Native where

import Common
import Data.Functor.Identity (Identity)
import LLVM.AST (Operand)
import LLVM.AST.IntegerPredicate as IP
import LLVM.IRBuilder (IRBuilderT, ModuleBuilderT, MonadIRBuilder)
import LLVM.IRBuilder.Instruction
import Stdlib (boolType)

type Builder = IRBuilderT (ModuleBuilderT Identity) Operand

type ArithInst = Operand -> Operand -> Builder

data NativeOp = ArithOp ArithInst | CompOp IP.IntegerPredicate

data NativeFun = NativeFun Id Type NativeOp

arithType = TFun (TBase TInt) (TFun (TBase TInt) (TBase TInt))

compType = TFun (TBase TInt) (TFun (TBase TInt) boolType)

nativeFuns :: [NativeFun]
nativeFuns =
  [ NativeFun "+" arithType (ArithOp add),
    NativeFun "-" arithType (ArithOp sub),
    NativeFun "*" arithType (ArithOp mul),
    NativeFun "/" arithType (ArithOp sdiv),
    NativeFun "==" compType (CompOp IP.EQ),
    NativeFun ">" compType (CompOp IP.SGT),
    NativeFun "<" compType (CompOp IP.SLT),
    NativeFun ">=" compType (CompOp IP.SGE),
    NativeFun "<=" compType (CompOp IP.SLE)
  ]
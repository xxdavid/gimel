module LlvmCommon where

import LLVM.AST
import LLVM.AST.Constant
import LLVM.AST.Type

data FunDecl = FunDecl Name [Type] Type

declToOperand (FunDecl name argtys retty) = funOperand name argtys retty

funOperand :: Name -> [Type] -> Type -> Operand
funOperand name argtys retty = ConstantOperand $ GlobalReference funty name
  where
    funty = funPtrType argtys retty

funPtrType :: [Type] -> Type -> Type
funPtrType argtys retty = ptr $ FunctionType retty argtys False
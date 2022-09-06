{-# LANGUAGE OverloadedStrings #-}

module Runtime where

import LLVM.AST (Operand)
import LLVM.AST.Type (Type (VoidType), i32, i8, ptr)
import LLVM.IRBuilder.Module (MonadModuleBuilder, extern)
import LlvmCommon
import Prelude hiding (lookup)

stackT :: Type
stackT = ptr i32

nodeT :: Type
nodeT = ptr i32

strT :: Type
strT = ptr i8

pushInt = FunDecl "push_int" [stackT, i32] stackT

pushGlobal = FunDecl "push_global" [stackT, i32, funPtrType [stackT] stackT] stackT

pushInstr = FunDecl "push_instr" [stackT, i32] stackT

push = FunDecl "push" [stackT, i32] stackT

mkApp = FunDecl "mk_app" [stackT] stackT

eval = FunDecl "eval" [stackT] stackT

update = FunDecl "update" [stackT, i32] stackT

pop = FunDecl "pop" [stackT, i32] stackT

slide = FunDecl "slide" [stackT, i32] stackT

pack = FunDecl "pack" [stackT, i32, i32] stackT

split = FunDecl "split" [stackT, i32] stackT

emptyStack = FunDecl "empty_stack" [] stackT

peek = FunDecl "peek" [stackT] stackT

lookup = FunDecl "lookup" [stackT, i32] stackT

unwind = FunDecl "unwind" [stackT] stackT

evalNode = FunDecl "eval_node" [nodeT] nodeT

getInt = FunDecl "get_int" [nodeT] i32

getConstrTag = FunDecl "get_constr_tag" [nodeT] i32

runtimeError = FunDecl "runtime_error" [stackT] VoidType

printNode = FunDecl "print_node" [nodeT] VoidType

printStack = FunDecl "print_stack_nolabel" [stackT] VoidType

printStr = FunDecl "print_str" [strT] VoidType

printIntNode = FunDecl "print_int_node" [stackT] VoidType

printLParen = FunDecl "print_lparen" [] VoidType

printRParen = FunDecl "print_rparen" [] VoidType

printSpace = FunDecl "print_space" [] VoidType

printNl = FunDecl "print_nl" [] VoidType

all =
  [ pushInt,
    pushGlobal,
    pushInstr,
    push,
    mkApp,
    eval,
    update,
    pop,
    slide,
    pack,
    split,
    emptyStack,
    peek,
    lookup,
    unwind,
    evalNode,
    getInt,
    getConstrTag,
    runtimeError,
    printNode,
    printStack,
    printStr,
    printIntNode,
    printLParen,
    printRParen,
    printSpace,
    printNl
  ]
module GMachine where

import Common
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Typer (predefinedTypes)

data Instr
  = PushInt Int
  | PushGlobal Id
  | Push Int
  | MkApp
  | Unwind
  | Eval
  | Jump [(Id, [Instr])]
  | Split Int
  | Slide Int
  | Update Int
  | Pop Int
  deriving (Show)

data StackItem = SVar Id | SOther deriving (Eq)

type StackEnv = [StackItem]

compileExpr :: PExpr -> StackEnv -> [Instr]
compileExpr (PNum _ n) _ = [PushInt n]
compileExpr (PApp _ a b) e = compileExpr b e ++ compileExpr a (SOther : e) ++ [MkApp]
compileExpr (PVar _ x) env = case maybeOffset of
  Just offset -> [Push offset]
  Nothing -> [PushGlobal x]
  where
    maybeOffset = elemIndex (SVar x) env
compileExpr (PCase _ m cls) env = compileExpr m env ++ [Eval, Jump (map compileClause cls)]
  where
    compileClause :: PClause -> (Id, [Instr])
    compileClause (PClause (PPVar _) _) = error "Variables are gonna be forbidden in clauses soon."
    compileClause (PClause (PPConstr constr vars) body) = (constr, instrs)
      where
        instrs = [Split arity] ++ compileExpr body env' ++ [Slide arity]
        arity = length vars
        env' = map SVar vars ++ env
compileExpr (PAbs {}) _ = error "Lambdas will be forbidden soon."

compileFun :: PFun -> (Id, [Instr])
compileFun (PFun name expr) = (name, instrs)
  where
    instrs = compileExpr innerExpr stackEnv ++ [Update arity, Pop arity]
    convertLambdas :: PExpr -> ([Id], PExpr)
    convertLambdas (PAbs _ x body) = (x : nextParams, body')
      where
        (nextParams, body') = convertLambdas body
    convertLambdas e = ([], e)
    (params, innerExpr) = convertLambdas expr
    arity = length params
    stackEnv = map SVar params

compileProg :: PProg -> [(Id, [Instr])]
compileProg prog = map compileFun (funs prog)
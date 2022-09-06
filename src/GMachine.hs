{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}

module GMachine where

import Common
import Data.List (elemIndex)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

type ConstrTag = Int

data Instr
  = PushInt Int
  | PushGlobal Id
  | Push Int
  | MkApp
  | Eval
  | Jump [(ConstrTag, [Instr])]
  | Pack ConstrTag Int
  | Split Int
  | Slide Int
  | Update Int
  | Pop Int
  deriving (Show)

data StackItem = SVar Id | SOther deriving (Eq)

type StackEnv = [StackItem]

type TagMap = Map.Map Id ConstrTag

data CompiledFun = CompiledFun Id [Instr] deriving (Show)

compileExpr :: PExpr -> TagMap -> StackEnv -> [Instr]
compileExpr (PNum _ n) _ _ = [PushInt n]
compileExpr (PApp _ a b) t e = compileExpr b t e ++ compileExpr a t (SOther : e) ++ [MkApp]
compileExpr (PVar _ x) _ env = case maybeOffset of
  Just offset -> [Push offset]
  Nothing -> [PushGlobal x]
  where
    maybeOffset = elemIndex (SVar x) env
compileExpr (PCase _ m cls) tagMap env = compileExpr m tagMap env ++ [Eval, Jump (map compileClause cls)]
  where
    compileClause :: PClause -> (ConstrTag, [Instr])
    compileClause (PClause (PPVar _) _) = error "Variables are now forbidden in clauses."
    compileClause (PClause (PPConstr constr vars) body) = (tag, instrs)
      where
        instrs = [Split arity] ++ compileExpr body tagMap env' ++ [Slide arity]
        arity = length vars
        env' = map SVar vars ++ env
        Just tag = Map.lookup constr tagMap
compileExpr (PAbs {}) _ _ = error "Lambdas are now forbidden."

compileFun :: PFun -> TagMap -> CompiledFun
compileFun (PFun name expr) tagMap = CompiledFun name instrs
  where
    instrs = compileExpr innerExpr tagMap stackEnv ++ [Update arity, Pop arity]
    (params, innerExpr) = lambdasToParams expr
    arity = length params
    stackEnv = map SVar params

tagConstructors :: [PData] -> TagMap
tagConstructors = foldr processData Map.empty
  where
    processData (PData _ constrs) mapping = foldr processConstr mapping (zip [0 ..] constrs)
    processConstr (tag, PConstr constr _) = Map.insert constr tag

compileConstr :: ConstrTag -> PConstr -> CompiledFun
compileConstr tag (PConstr constr params) = CompiledFun constr instrs
  where
    instrs = [Pack tag (length params), Update 0]

compileData :: PData -> [CompiledFun]
compileData (PData _ constrs) = zipWith compileConstr [0 ..] constrs

compileProg :: PProg -> [CompiledFun]
compileProg prog = funInstrs ++ dataInstrs
  where
    funInstrs = map (flip compileFun tagMap) (funs prog)
    dataInstrs = concatMap compileData (datas prog)
    tagMap = tagConstructors (datas prog)
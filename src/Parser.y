{
module Parser(parse, parseExpr) where

import Common
import Lexer
import Data.Maybe(catMaybes)
}

%name parse Prog
%name parseExpr TopExpr
%tokentype { Token }
%error { parseError }

%expect 0

%token
    '='           { LAssign }
    '+'           { LPlus }
    '-'           { LMinus }
    '*'           { LStar }
    '/'           { LSlash }
    '\\'          { LBackslash }
    '('           { LLParen }
    ')'           { LRParen }
    '->'          { LArrow }
    '|'           { LPipe }
    data          { LData }
    do            { LDo }
    end           { LEnd }
    case          { LCase }
    id            { LId $$ }
    Id            { LUpperId $$ }
    num           { LNum $$ }
    nl            { LNewLine }

%left '+' '-'
%left '*' '/'

%%

Prog    :: { PProg }
Prog    : Defs                          { classifyDefs $1 }

Defs    :: { [PDef] }
Defs    : Defs_                         { reverse $1 }

Defs_   :: { [PDef] }
Defs_   : Def                           { [$1] }
        | Defs_ Nls Def                 { $3 : $1 }

Def     :: { PDef }
Def     : id Params '=' TopExpr         { PDFun $ PFun $1 (createAbs $2 $4) }
        | data Id '=' Constrs           { PDData $ PData $2 $4 }

Params  :: { [Id] }
Params  : Params_                       { reverse $1 }

Params_ :: { [Id] }
Params_ :                               { [] }
        | Params_ id                    { $2 : $1 }

TopExpr :: { PExpr }
TopExpr : TopExpr '+' TopExpr           { PApp ann (PApp ann (PVar ann "+") $1) $3 }
        | TopExpr '-' TopExpr           { PApp ann (PApp ann (PVar ann "-") $1) $3 }
        | TopExpr '*' TopExpr           { PApp ann (PApp ann (PVar ann "*") $1) $3 }
        | TopExpr '/' TopExpr           { PApp ann (PApp ann (PVar ann "/") $1) $3 }
        | Expr                          { $1 }
        | Case                          { $1 }

Expr    :: { PExpr }
Expr    : Base                          { $1 }
        | Expr Base                     { PApp ann $1 $2 }

Base    :: { PExpr }
Base    : num                           { PNum ann $1 }
        | id                            { PVar ann $1 }
        | Id                            { PVar ann $1 }
        | '(' TopExpr ')'               { $2 }
        | '(' '\\' Params '->' TopExpr ')' { createAbs $3 $5 }

Case    :: { PExpr }
Case    : case TopExpr do nl Clauses nl end { PCase ann $2 (reverse $5) }

-- When reversing via a new non-terminal, it reports a shift-reduce conflict.
Clauses :: { [PClause] }
Clauses : Clause                        { [$1] }
        | Clauses nl Clause             { $3 : $1 }

Clause  :: { PClause }
Clause  :  Pattern '->' TopExpr         { PClause $1 $3 }

Pattern :: { PPattern }
Pattern : id                            { PPVar $1 }
        | Id Params                     { PPConstr $1 $2 }

Patterns:: { [PPattern] }
Patterns: Patterns_                     { reverse $1 }

Patterns_:: { [PPattern] }
Patterns_:                              { [] }
         | Patterns_ Pattern            { $2 : $1 }

Constrs :: { [PConstr] }
Constrs : Constrs_                      { reverse $1 }

Constrs_:: { [PConstr] }
Constrs_: Constr                        { [$1] }
        | Constrs_ '|' Constr           { $3 : $1 }

Constr  :: { PConstr }
Constr  : Id Types                      { PConstr $1 $2 }

Types   :: { [Type] }
Types   : Types_                        { reverse $1 }

Types_  :: { [Type] }
Types_  :                               { [] }
        | Types_ Type                   { $2 : $1 }

Type    :: { Type }
Type    : Id                            { parseType $1 }

Nls     :: { () }
Nls     : nl                            { () }
        | Nls nl                        { () }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

createAbs :: [Id] -> PExpr -> PExpr
createAbs [] body = body
createAbs (x:xs) body = PAbs ann x (createAbs xs body)

parseType :: String -> Type
parseType "Int" = TBase TInt
parseType id = TData id

classifyDefs :: [PDef] -> PProg
classifyDefs defs = PProg (catMaybes $ map getFun defs) (catMaybes $ map getData defs) where
        getFun (PDFun x) = Just x
        getFun _ = Nothing
        getData (PDData x) = Just x
        getData _ = Nothing

ann = emptyAnn

}

{
module Parser (PDef(..), PExpr(..), Id, parse) where

import Lexer
import Types
}

%name parse
%tokentype { Token }
%error { parseError }

%expect 0

%token
    '='           { TAssign }
    '+'           { TPlus }
    '-'           { TMinus }
    '*'           { TStar }
    '/'           { TSlash }
    '\\'          { TBackslash }
    '('           { TLParen }
    ')'           { TRParen }
    '->'          { TArrow }
    id            { TId $$ }
    num           { TNum $$ }
    nl            { TNewLine }

%left '+' '-'
%left '*' '/'

%%

Defs    :: { [PDef] }
Defs    : Defs_                         { reverse $1 }

Defs_   :: { [PDef] }
Defs_   : Def                           { [$1] }
        | Defs_ nl Def                  { $3 : $1 }

Def     :: { PDef }
Def     : id Params '=' TopExpr         { PDef $1 (createAbs $2 $4) }

Params  :: { [Id] }
Params  : Params_                       { reverse $1 }

Params_ :: { [Id] }
Params_ :                               { [] }
        | Params_ id                    { $2 : $1 }

TopExpr :: { PExpr }
TopExpr : TopExpr '+' TopExpr           { PApp [] (PApp [] (PVar [] "+") $1) $3 }
        | TopExpr '-' TopExpr           { PApp [] (PApp [] (PVar [] "-") $1) $3 }
        | TopExpr '*' TopExpr           { PApp [] (PApp [] (PVar [] "*") $1) $3 }
        | TopExpr '/' TopExpr           { PApp [] (PApp [] (PVar [] "/") $1) $3 }
        | Expr                          { $1 }

Expr    :: { PExpr }
Expr    : Base                          { $1 }
        | Expr Base                     { PApp [] $1 $2 }

Base    :: { PExpr }
Base    : num                           { PNum [] $1 }
        | id                            { PVar [] $1 }
        | '(' TopExpr ')'               { $2 }
        | '(' '\\' Params '->' TopExpr ')' { createAbs $3 $5 }



{
parseError :: [Token] -> a
parseError _ = error "Parse error"

type Id = String

data PDef
    = PDef Id PExpr
    deriving (Eq, Show)

data Annotation = AType Type deriving (Eq, Show)
type Ann = [Annotation]

data PExpr
    = PNum Ann Int
    | PApp Ann PExpr PExpr
    | PVar Ann Id
    | PAbs Ann Id PExpr
    deriving (Eq, Show)

createAbs :: [Id] -> PExpr -> PExpr
createAbs [] body = body
createAbs (x:xs) body = PAbs [] x (createAbs xs body)

}

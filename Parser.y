{
module Parser (PDef(..), PExpr(..), Id, parse) where

import Lexer
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
    '('           { TLParen }
    ')'           { TRParen }
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
Def     : id Params '=' TopExpr         { PDef $1 $2 $4 }

Params  :: { [Id] }
Params  : Params_                       { reverse $1 }

Params_ :: { [Id] }
Params_ :                               { [] }
        | Params_ id                    { $2 : $1 }

TopExpr :: { PExpr }
TopExpr : TopExpr '+' TopExpr           { PApp (PApp (PVar "+") $1) $3 }
        | TopExpr '-' TopExpr           { PApp (PApp (PVar "-") $1) $3 }
        | TopExpr '*' TopExpr           { PApp (PApp (PVar "*") $1) $3 }
        | TopExpr '/' TopExpr           { PApp (PApp (PVar "/") $1) $3 }
        | Expr                          { $1 }

Expr    :: { PExpr }
Expr    : Base                          { $1 }
        | Expr Base                     { PApp $1 $2 }

Base    :: { PExpr }
Base    : num                           { PNum $1 }
        | id                            { PVar $1 }
        | '(' TopExpr ')'               { $2 }


{
parseError :: [Token] -> a
parseError _ = error "Parse error"

type Id = String

data PDef
    = PDef Id [Id] PExpr
    deriving (Eq, Show)

data PExpr
    = PNum Int
    | PApp PExpr PExpr
    | PVar Id
    deriving (Eq, Show)
}

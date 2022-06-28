{
module Lexer (alexScanTokens, Token(..)) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-
    \=                      { \s -> TAssign }
    \+                      { \s -> TPlus }
    \-                      { \s -> TMinus }
    \*                      { \s -> TStar }
    \/                      { \s -> TSlash }
    \(                      { \s -> TLParen }
    \)                      { \s -> TRParen }
    $alpha+                 { \s -> TId s }
    $digit+                 { \s -> TNum (read s) }
    \n+                     { \s -> TNewLine }
    $white                  ;
    \#.*                    ;

{
data Token
    = TId String
    | TNum Int
    | TPlus
    | TMinus
    | TStar
    | TSlash
    | TAssign
    | TLParen
    | TRParen
    | TNewLine
    deriving (Eq, Show)
}

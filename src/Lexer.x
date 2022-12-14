{
module Lexer (tokenize, Token(..)) where
import Data.List (dropWhileEnd)
}

%wrapper "posn"

$digit = 0-9
$lower = [a-z]
$upper = [A-Z]
$letter = [a-zA-Z]
$enhLetter = [a-zA-Z0-9_\']

tokens :-
    \=                      { \_ s -> LAssign }
    \+                      { \_ s -> LPlus }
    \-                      { \_ s -> LMinus }
    \*                      { \_ s -> LStar }
    \/                      { \_ s -> LSlash }
    \=\=                    { \_ s -> LEqual }
    \>                      { \_ s -> LGreater }
    \<                      { \_ s -> LLower }
    \>=                     { \_ s -> LGreaterEq }
    \<=                     { \_ s -> LLowerEq }
    \\                      { \_ s -> LBackslash }
    \(                      { \_ s -> LLParen }
    \)                      { \_ s -> LRParen }
    \|                      { \_ s -> LPipe }
    \->                     { \_ s -> LArrow }
    data                    { \_ s -> LData }
    do                      { \_ s -> LDo }
    end                     { \_ s -> LEnd }
    case                    { \_ s -> LCase }
    $lower$enhLetter*       { \_ s -> LId s }
    $upper$enhLetter*       { \_ s -> LUpperId s }
    $digit+                 { \_ s -> LNum (read s) }
    \n+                     { \_ s -> LNewLine }
    $white                  ;
    \#.*                    ;

{
data Token
    = LId String
    | LUpperId String
    | LNum Int
    | LPlus
    | LMinus
    | LStar
    | LSlash
    | LEqual
    | LGreater
    | LLower
    | LGreaterEq
    | LLowerEq
    | LBackslash
    | LAssign
    | LLParen
    | LRParen
    | LArrow
    | LPipe
    | LData
    | LDo
    | LEnd
    | LCase
    | LNewLine
    deriving (Eq, Show)

tokenize = trim . alexScanTokens
    where
        trim = dropWhileEnd isNl . dropWhile isNl
        isNl token = token == LNewLine
}

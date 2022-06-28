module Main where

import Lexer
import Parser

deleteLast [h] = []
deleteLast (x:xs) = x : deleteLast xs

main = do
    s <- getContents
    let tokens = alexScanTokens (deleteLast s)
    print tokens
    let ast = parse tokens
    print ast

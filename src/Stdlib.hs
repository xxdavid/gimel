module Stdlib where

import Common
import Paths_gimel
import System.Directory (getCurrentDirectory)

readStdlib :: IO String
readStdlib = getDataFileName "assets/stdlib.gm" >>= readFile

boolType = TData "Bool"
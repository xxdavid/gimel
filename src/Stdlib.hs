module Stdlib where

import Common
import Paths_gimel

readStdlib :: IO String
readStdlib = getDataFileName "assets/stdlib.gm" >>= readFile

boolType = TData "Bool"
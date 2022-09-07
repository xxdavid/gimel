{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Config where

import System.Console.CmdArgs

data Config = Config {src :: FilePath, out :: Maybe FilePath, run :: Bool, verbose :: Bool}
  deriving (Show, Data, Typeable)
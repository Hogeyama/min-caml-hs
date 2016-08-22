
module Main where

import AllTypes
import Type
import Lexer
import Parser
import Typing
import KNormal
import System.IO (Handle, stdin, hGetContents, openFile, IOMode(..))

main :: IO ()
main = do
  print $ Just $ Just $ Just $ Just 1

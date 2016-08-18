{-# LANGUAGE LambdaCase #-}

import CamlMonad
import Type
import Syntax
import Lexer
import Parser
import Typing
import KNormal
import Alpha
import Optimise
import System.IO (Handle, stdin, hGetContents, openFile, IOMode(..))
default(Int)

limit :: Int
limit = 100

test :: FilePath -> IO ()
test f = do
  s <- readFile f
  let m = scanTokens s
  case m of
    Left e -> print e
    Right tks -> do
      m <- runCaml $ do
          e <- parse tks
          e <- typing e
          e <- kNormalize e
          e <- alpha e
          e <- optimise limit e
          return e
      case m of
        Left err -> print err
        Right e  -> print e

main :: IO ()
main = test "test/test.ml"

      --m <- runCaml $ parse tks
      --     >>= typing
      --     >>= kNormalize
      --     >>= alpha
      --     >>= optimise 1000


{-# LANGUAGE LambdaCase #-}

import CamlMonad
import Type
import Syntax
import Lexer
import Parser
import Typing
import KNormal
import Alpha
import Beta
import Assoc
import Inline
import System.IO (Handle, stdin, hGetContents, openFile, IOMode(..))
default(Int)

limit :: Int
limit = 1

iter :: Int -> KExpr -> Caml KExpr
iter 0 e = return e
iter n e = do
  e' <- inline =<< assoc =<< beta e
  if e==e'
    then return e
    else iter (n-1) e'

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
          {-e <- iter limit e-}
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
      --     >>= iter 1000

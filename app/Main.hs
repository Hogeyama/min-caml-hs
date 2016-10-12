{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import AllTypes (runCaml, initialState, S(..))
import Lexer    (lex)
import Parser   (parse)
import Typing   (typing)
import KNormal  (kNormalize)
import Alpha    (alpha)
import Optimise (optimise)
import Closure  (closureConvert)
import Virtual  (virtualCode)
import RegAlloc (regAlloc)
import Simm     (simm)
import Emit     (emit)

import Prelude hiding (lex)
import System.IO (withFile, IOMode(..))
import Options

main :: IO ()
main = runCommand $ \opts args -> do
  let s = initialState { _threshold = inline opts
                       , _optimiseLimit = iter opts}
  mapM_ (file s) args

file :: S -> FilePath -> IO ()
file s f = do
  str <- readFile (f ++ ".ml")
  withFile (f ++ ".s") WriteMode $ \out -> do
    m <-(`runCaml` s) $ lex str
          >>= parse
          >>= typing
          >>= kNormalize
          >>= alpha
          >>= optimise
          >>= closureConvert
          >>= virtualCode
          >>= simm
          >>= regAlloc
          >>= emit out
    case m of
      Right () -> return ()
      Left e -> error $ f ++ ": " ++ show e

data MinCamlOptions = MinCamlOptions
                    { inline :: Int
                    , iter   :: Int
                    }
instance Options MinCamlOptions where
  defineOptions = pure MinCamlOptions
               <*> simpleOption "inline"
                   0 "maximum size of functions inlined"
               <*> simpleOption "iter"
                   100 "maximum number of optimizations iterated"


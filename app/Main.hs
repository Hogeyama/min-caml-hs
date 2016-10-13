{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Base                 (runCaml, initialState, S(..))
import FrontEnd.Lexer       (lex)
import FrontEnd.Parser      (parse)
import FrontEnd.Typing      (typing)
import MiddleEnd.KNormal    (kNormalize)
import MiddleEnd.Alpha      (alpha)
import MiddleEnd.Optimise   (optimise)
import MiddleEnd.Closure    (closureConvert)
import BackEnd.X86.Virtual  (virtualCode)
import BackEnd.X86.RegAlloc (regAlloc)
import BackEnd.X86.Simm     (simm)
import BackEnd.X86.Emit     (emit)

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


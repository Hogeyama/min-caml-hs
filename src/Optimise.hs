
module Optimise where

import CamlMonad
import KNormal
import Beta
import Assoc
import Inline
import ConstFold
import Elim

optimise :: Int -> KExpr -> Caml KExpr
optimise 0 e = return e
optimise n e = do
  e' <- elim =<< constFold =<< inline =<< assoc =<< beta e
  if e==e'
    then return e
    else optimise (n-1) e'


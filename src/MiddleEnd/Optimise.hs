
module MiddleEnd.Optimise where

import Base
import MiddleEnd.KNormal
import MiddleEnd.Beta
import MiddleEnd.Assoc
import MiddleEnd.Inline
import MiddleEnd.ConstFold
import MiddleEnd.Elim
import Control.Lens (use)

optimise :: KExpr -> Caml KExpr
optimise e = do
  n <- use optimiseLimit
  optimise' n e

optimise' :: Int -> KExpr -> Caml KExpr
optimise' 0 e = return e
optimise' n e = do
  e' <- elim =<< constFold =<< inline =<< assoc =<< beta e
  if e==e'
    then return e
    else optimise' (n-1) e'


{-# LANGUAGE LambdaCase #-}

module Assoc where

import KNormal
import AllTypes

assoc :: KExpr -> Caml KExpr
assoc = return . g

g :: KExpr -> KExpr
g = \case
  KIfEq x y e1 e2 -> KIfEq x y (g e1) (g e2)
  KIfLe x y e1 e2 -> KIfLe x y (g e1) (g e2)
  KLet xt e1 e2 ->
    let insert = \case
          KLet yt e3 e4 -> KLet yt e3 (insert e4)
          KLetRec fundef e -> KLetRec fundef (insert e)
          KLetTuple yts z e -> KLetTuple yts z (insert e)
          e -> KLet xt e (g e2)
    in  insert (g e1)
  KLetTuple xts y e -> KLetTuple xts y (g e)
  KLetRec (KFunDef xt yts e1) e2 -> KLetRec (KFunDef xt yts (g e1)) (g e2)
  e -> e


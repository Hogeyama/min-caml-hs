{-# LANGUAGE TemplateHaskell #-}

module FrontEnd.Syntax where

import Base
import Control.Lens

---------------------
-- Syntax.t = Expr --
---------------------
data Expr = EUnit
          | EBool     Bool
          | EInt      Int
          | EFloat    Double
          | ENot      Expr
          | ENeg      Expr
          | EAdd      Expr Expr
          | ESub      Expr Expr
          | EFNeg     Expr
          | EFAdd     Expr Expr
          | EFSub     Expr Expr
          | EFMul     Expr Expr
          | EFDiv     Expr Expr
          | EEq       Expr Expr
          | ELe       Expr Expr
          | EIf       Expr Expr Expr
          | ELet      (Id, Type) Expr Expr
          | EVar      Id
          | ELetRec   EFunDef Expr
          | EApp      Expr [Expr]
          | ETuple    [Expr]
          | ELetTuple [(Id,Type)] Expr Expr
          | EArray    Expr Expr
          | EGet      Expr Expr
          | EPut      Expr Expr Expr
          deriving (Show, Eq)

data EFunDef = EFunDef { _ename :: (Id, Type)
                       , _eargs :: [(Id,Type)]
                       , _ebody :: Expr
                       }
            deriving (Show, Eq)
makeLenses ''EFunDef


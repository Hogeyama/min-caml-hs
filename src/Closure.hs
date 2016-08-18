{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Closure where

import CamlMonad
import Id
import KNormal
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Lens

data Closure = Closure { _entry    :: Label
                       , _actualFV :: [Id]}
-- Closure変換後の式
data CExpr = CUnit
           | CInt      Int
           | CFloat    Double
           | CNeg      Id
           | CAdd      Id Id
           | CSub      Id Id
           | CFNeg     Id
           | CFAdd     Id Id
           | CFSub     Id Id
           | CFMul     Id Id
           | CFDiv     Id Id
           | CIfEq     Id Id     CExpr CExpr
           | CIfLE     Id Id     CExpr CExpr
           | CLet      (Id,Type) CExpr CExpr
           | CVar      Id
           | CMakeCls  (Id,Type) Closure CExpr
           | CAppCls   Id [Id]
           | CAppDir   Label [Id]
           | CTuple    [Id]
           | CLetTuple [(Id,Type)] Id CExpr
           | CGet      Id Id
           | CPut      Id Id Id
           | CExtArray Label
data CFunDef = CFunDef { _cname     :: (Label,Type)
                       , _cargs     :: [(Id,Type)]
                       , _cFormalFV :: [(Id,Type)]
                       , _cbody     :: CExpr}
data CProg = CProg ([CFunDef], CExpr)

makeLenses ''Closure
makeLenses ''CFunDef






module MiddleEnd.Closure where

import {-# SOURCE #-} Base

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
           | CIfLe     Id Id     CExpr CExpr
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
data Closure = Closure { _entry    :: Label
                       , _actualFV :: [Id]}
data CProg = CProg [CFunDef] CExpr
data CFunDef = CFunDef { _cname     :: (Label,Type)
                       , _cargs     :: [(Id,Type)]
                       , _cFormalFV :: [(Id,Type)]
                       , _cbody     :: CExpr}
instance Show CFunDef


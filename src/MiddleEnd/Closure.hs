{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{- KExpr -> CExpr -}

module MiddleEnd.Closure (
  CExpr(..),
  CProg(..),
  CFunDef(..),
  Closure(..),
  closureConvert,
  fv
) where

import Base
import MiddleEnd.KNormal hiding (fv)

import           Data.Map       (Map)
import qualified Data.Map as M
import           Data.Set       (Set)
import qualified Data.Set as S
import qualified Data.List as L
import           Control.Lens
import           Data.Maybe     (fromJust)

-----------------------
-- Closure.t = CExpr --
-----------------------
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
           deriving Show
data Closure = Closure { _entry    :: Label
                       , _actualFV :: [Id]}
             deriving Show
data CProg = CProg [CFunDef] CExpr
           deriving Show
data CFunDef = CFunDef { _cname     :: (Label,Type)
                       , _cargs     :: [(Id,Type)]
                       , _cFormalFV :: [(Id,Type)]
                       , _cbody     :: CExpr}
              deriving Show
{-makeLenses ''CFunDef-}

closureConvert :: KExpr -> Caml CProg
closureConvert e = do
  closureToplevel .= []
  e' <- g M.empty S.empty e
  toplevel <- use closureToplevel
  return $ CProg (reverse toplevel) e'

fv :: CExpr -> Set Id
fv = \case
  CUnit       -> S.empty
  CInt{}      -> S.empty
  CFloat{}    -> S.empty
  CExtArray{} -> S.empty

  CNeg  x -> S.singleton x
  CFNeg x -> S.singleton x
  CVar  x -> S.singleton x

  CAdd  x y -> S.fromList [x,y]
  CSub  x y -> S.fromList [x,y]
  CFAdd x y -> S.fromList [x,y]
  CFSub x y -> S.fromList [x,y]
  CFMul x y -> S.fromList [x,y]
  CFDiv x y -> S.fromList [x,y]
  CGet  x y -> S.fromList [x,y]

  CIfEq x y e1 e2 -> S.insert x $ S.insert y $ S.union (fv e1) (fv e2)
  CIfLe x y e1 e2 -> S.insert x $ S.insert y $ S.union (fv e1) (fv e2)

  CLet (x,_t) e1 e2 -> S.union (fv e1) (S.delete x (fv e2))

  CMakeCls (x,_t) (Closure _l ys) e -> S.delete x (S.union (S.fromList ys) (fv e))

  CAppCls x ys -> S.fromList $ x:ys
  CAppDir _ xs -> S.fromList xs

  CTuple xs -> S.fromList xs

  CLetTuple xts y e -> S.insert y $ (fv e) S.\\ (S.fromList (map fst xts))

  CPut x y z -> S.fromList [x,y,z]


-- known = known to be able to apply directly
g :: Map Id Type -> Set Id -> KExpr -> Caml CExpr
g env known = \case
  KUnit    -> return $ CUnit
  KInt i   -> return $ CInt i
  KFloat d -> return $ CFloat d

  KNeg  x -> return $ CNeg x
  KFNeg x -> return $ CFNeg x

  KAdd  x y -> return $ CAdd  x y
  KSub  x y -> return $ CSub  x y
  KFAdd x y -> return $ CFAdd x y
  KFSub x y -> return $ CFSub x y
  KFMul x y -> return $ CFMul x y
  KFDiv x y -> return $ CFDiv x y

  KIfEq x y e1 e2 -> CIfEq x y <$> g env known e1 <*> g env known e2
  KIfLe x y e1 e2 -> CIfLe x y <$> g env known e1 <*> g env known e2

  KLet (x,t) e1 e2 -> CLet (x,t) <$> g env known e1 <*> g (M.insert x t env) known e2

  KVar x -> return $ CVar x

  KLetRec (KFunDef (x,t) yts e1) e2 -> do
    -- xは自由変数を持たないと仮定してとりあえずやってみる
    -- だめだったらやり直す
    toplevelBackup <- use closureToplevel
    let env'     = M.insert x t env
        env''    = M.union (M.fromList yts) env'
        known'   = S.insert x known
        (ys,_ts) = unzip yts
    e1' <- g env'' known' e1

    -- かくにん
    (known'', e1'') <- case S.toList (fv e1') L.\\ ys of
        [] -> return (known', e1')   -- OK
        zs -> do liftIO $ putStrLn $ -- NG
                      "free variable(s) " ++ ppList zs ++ " " ++
                      "found in function " ++ x ++ "\n" ++
                      "function " ++ x ++ " cannot be directly applied in fact"
                 closureToplevel .= toplevelBackup
                 e1'' <- g env'' known e1
                 return (known, e1'')
    let zs'  = S.toList (fv e1'') L.\\ (x:ys)
        zts' = [(z, fromJust (M.lookup z env')) | z <- zs']
    closureToplevel %= (CFunDef (Label x, t) yts zts' e1'' :) -- 追加
    e2' <- g env' known'' e2
    if S.member x (fv e2') then
        return $ CMakeCls (x,t) (Closure (Label x) zs') e2'
    else do
        liftIO $ putStrLn $ "eliminating closure(s) " ++ x
        return e2'

  KApp x ys
    | S.member x known -> do
        liftIO $ putStrLn $ "directly applying " ++ x
        return $ CAppDir (Label x) ys
    | otherwise ->
        return $ CAppCls x ys

  KTuple xs -> return $ CTuple xs

  KLetTuple xts y e -> CLetTuple xts y <$> g (M.union (M.fromList xts) env) known e

  KGet x y        -> return $ CGet x y
  KPut x y z      -> return $ CPut x y z
  KExtArray x     -> return $ CExtArray (Label x)
  KExtFunApp x ys -> return $ CAppDir (Label ("min_caml_" ++ x)) ys


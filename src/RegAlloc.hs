{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RegAlloc where

import Prelude hiding (exp)
import Id
import Asm
import AllTypes

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Vector (Vector, (!))
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Foldable (foldlM)
import Control.Exception.Base (assert)
import qualified Data.Foldable as F


target' :: Id -> (Id,Type) -> AExpr -> (Bool, [Id])
target' src (dest,t) = \case
  AMov x
    | x==src && isReg dest -> case t of
        TUnit  -> error $ "RegAlloc.target' AMov " ++ x ++ " TUnit"
        TFloat -> error $ "RegAlloc.target' AMov " ++ x ++ " TFloat"
        _ -> (False, [dest])
  AFMovD x
    | x==src && isReg dest -> case t of
        TFloat -> (False, [dest])
        _ -> error "RegAlloc.target' AFMovD"

  AIfEq  _ _ e1 e2 ->
      let (c1,rs1) = target src (dest,t) e1
          (c2,rs2) = target src (dest,t) e2
      in  (c1&&c2, rs1++rs2)
  AIfLe  _ _ e1 e2 ->
      let (c1,rs1) = target src (dest,t) e1
          (c2,rs2) = target src (dest,t) e2
      in  (c1&&c2, rs1++rs2)
  AIfGe  _ _ e1 e2 ->
      let (c1,rs1) = target src (dest,t) e1
          (c2,rs2) = target src (dest,t) e2
      in  (c1&&c2, rs1++rs2)
  AIfFEq _ _ e1 e2 ->
      let (c1,rs1) = target src (dest,t) e1
          (c2,rs2) = target src (dest,t) e2
      in  (c1&&c2, rs1++rs2)
  AIfFLe _ _ e1 e2 ->
      let (c1,rs1) = target src (dest,t) e1
          (c2,rs2) = target src (dest,t) e2
      in  (c1&&c2, rs1++rs2)

  ACallCls x ys zs ->
      (True, targetArgs src  regs 0 ys ++
             targetArgs src fregs 0 zs ++
             if x == src then [regCl] else [])

  ACallDir _ ys zs ->
      (True, targetArgs src  regs 0 ys ++
             targetArgs src fregs 0 zs)

  _ -> (False, [])


target :: Id -> (Id, Type) -> Asm -> (Bool, [Id])
target src destt = \case
  AsmAns exp -> target' src destt exp
  AsmLet xt exp e ->
    let (c1,rs1) = target' src xt exp
    in  if c1 then (True, rs1)
              else let (c2,rs2) = target src destt e
                   in  (c2, rs1++rs2)

targetArgs :: Id -> Vector Id -> Int -> [Id] -> [Id]
targetArgs src regs' n = \case
  [] -> []
  y:ys
    | y == src  -> regs'!n : targetArgs src regs' (n+1) ys
    | otherwise -> targetArgs src regs' (n+1) ys

source' :: Type -> AExpr -> [Id]
source' t = \case
  AMov x       -> [x]
  ANeg x       -> [x]
  AAdd x (C _) -> [x]
  ASub x _     -> [x]
  AFMovD x     -> [x]
  AFNegD x     -> [x]
  AFSubD x _   -> [x]
  AFDivD x _   -> [x]
    -- TODO なんで AddとSubで扱いが違うんじゃ 他
  AAdd x (V y) -> [x,y]
  AFAddD x y   -> [x,y]
  AFMulD x y   -> [x,y]

  AIfEq  _ _ e1 e2 -> source t e1 ++ source t e2
  AIfLe  _ _ e1 e2 -> source t e1 ++ source t e2
  AIfFEq _ _ e1 e2 -> source t e1 ++ source t e2
  AIfFLe _ _ e1 e2 -> source t e1 ++ source t e2

  ACallCls{} -> case t of TUnit -> []; TFloat -> [fregs!0]; _ -> [regs!0]
  ACallDir{} -> case t of TUnit -> []; TFloat -> [fregs!0]; _ -> [regs!0]

  _ -> []

source :: Type -> Asm -> [Id]
source t = \case
  AsmAns exp -> source' t exp
  AsmLet _ _ e -> source t e

data AllocResult = Alloc Id
                 | Spill Id

alloc :: Asm -> Map Id Id -> Id -> Type -> [Id] -> Caml AllocResult
alloc cont regenv x t prefer = assert (M.notMember x regenv) $
  let allregs = case t of
              TUnit -> [] --dummy
              TFloat -> allFRegs
              _ -> allRegs
  in if | null allregs -> return $ Alloc "%unit" -- Ad hoc (とは)
        | isReg x      -> return $ Alloc x
        | otherwise    ->
            let free = fv cont
                live = foldl' func S.empty free
                   where
                     func l y
                       | isReg y   = S.insert y l
                       | otherwise = S.insert (M.findWithDefault y y regenv) l
            in case F.find (`S.notMember` live) (prefer++allregs) of
                 Just r  -> return $ Alloc r
                 Nothing -> let y = fromJust $ F.find p (reverse free)
                                  where p y' = case M.lookup y' regenv of
                                                Just r -> not (isReg y') && r`elem`allregs
                                                Nothing -> False
                                msg = "spilling " ++ y ++ " from "
                                    ++ show (lookupJust y regenv)
                            in liftIO (putStrLn msg) >> return (Spill y)

add :: Id -> Id -> Map Id Id -> Map Id Id
add x r regenv
  | isReg x   = assert (x==r) regenv
  | otherwise = M.insert x r regenv

find :: Id -> Type -> Map Id Id -> Caml Id
find x t regenv
  | isReg x           = return x
  | M.member x regenv = return (lookupJust x regenv)
  | otherwise         = throw (NoReg x t)

find' :: IdOrImm -> Map Id Id -> Caml IdOrImm
find' (V x) regenv = V <$> find x TInt regenv
find' c _ = return c

g :: (Id,Type) -> Asm -> Map Id Id -> Asm -> Caml (Asm, Map Id Id)
g destt cont regenv = \case
  AsmAns exp -> g'_and_restore destt cont regenv exp
  AsmLet xt@(x,t) exp e -> assert (M.notMember x regenv) $ do
    let cont' = concat' e destt cont
    (e1',regenv1) <- g'_and_restore xt cont' regenv exp
    let (_call,targets) = target x destt cont'
        sources = source t e1'
    m <- alloc cont' regenv1 x t (targets++sources)
    case m of
      Spill y -> do
          let r = lookupJust y regenv1
          (e2',regenv2) <- g destt cont (add x r (M.delete y regenv1)) e
          let save = case M.lookup y regenv of
                       Just r'  -> ASave r' y
                       Nothing -> ANop
          e' <- seq' (save, concat' e1' (r,t) e2')
          return (e', regenv2)
      Alloc r -> do
          (e2', regenv2) <- g destt cont (add x r regenv1) e
          return $ (concat' e1' (r, t) e2', regenv2)

g' :: (Id,Type) -> Asm -> Map Id Id -> AExpr -> Caml (Asm, Map Id Id)
g' destt cont regenv exp = case exp of
  ANop       -> return (AsmAns exp, regenv)
  ASet{}     -> return (AsmAns exp, regenv)
  ASetL{}    -> return (AsmAns exp, regenv)
  AComment{} -> return (AsmAns exp, regenv)
  ARestore{} -> return (AsmAns exp, regenv)

  AMov x -> do
      rx <- find x TInt regenv
      return (AsmAns (AMov rx), regenv)
  ANeg x -> do
      rx <- find x TInt regenv
      return (AsmAns (ANeg rx), regenv)

  AAdd x y' -> do
      rx <- find  x  TInt regenv
      ry'<- find' y' regenv
      return (AsmAns (AAdd rx ry'), regenv)
  ASub x y' -> do
      rx <- find  x  TInt regenv
      ry'<- find' y' regenv
      return (AsmAns (ASub rx ry'), regenv)

  ALd x y' i -> do
      rx <- find  x  TInt regenv
      ry'<- find' y' regenv
      return (AsmAns (ALd rx ry' i), regenv)
  ASt x y z' i -> do
      rx <- find  x  TInt regenv
      ry <- find  y  TInt regenv
      rz'<- find' z' regenv
      return (AsmAns (ASt rx ry rz' i), regenv)

  AFMovD x -> do
      rx <- find x TFloat regenv
      return (AsmAns (AFMovD rx), regenv)
  AFNegD x -> do
      rx <- find x TFloat regenv
      return (AsmAns (AFNegD rx), regenv)

  AFAddD x y -> do
      rx <- find x TFloat regenv
      ry <- find y TFloat regenv
      return (AsmAns (AFAddD rx ry), regenv)
  AFSubD x y -> do
      rx <- find x TFloat regenv
      ry <- find y TFloat regenv
      return (AsmAns (AFSubD rx ry), regenv)
  AFMulD x y -> do
      rx <- find x TFloat regenv
      ry <- find y TFloat regenv
      return (AsmAns (AFMulD rx ry), regenv)
  AFDivD x y -> do
      rx <- find x TFloat regenv
      ry <- find y TFloat regenv
      return (AsmAns (AFDivD rx ry), regenv)

  ALdDF x y' i -> do
      rx <- find  x  TInt regenv
      ry'<- find' y' regenv
      return (AsmAns (ALdDF rx ry' i), regenv)

  AStDF x y z' i -> do
      rx <- find  x  TFloat regenv
      ry <- find  y  TInt   regenv
      rz'<- find' z' regenv
      return (AsmAns (AStDF rx ry rz' i), regenv)

  AIfEq x y' e1 e2 -> do
      rx <- find  x  TInt regenv
      ry'<- find' y' regenv
      g'_if destt cont regenv exp (AIfEq rx ry') e1 e2
  AIfLe x y' e1 e2 -> do
      rx <- find  x  TInt regenv
      ry'<- find' y' regenv
      g'_if destt cont regenv exp (AIfLe rx ry') e1 e2
  AIfGe x y' e1 e2 -> do
      rx <- find  x  TInt regenv
      ry'<- find' y' regenv
      g'_if destt cont regenv exp (AIfGe rx ry') e1 e2

  AIfFEq x y e1 e2 -> do
      rx <- find x TFloat regenv
      ry <- find y TFloat regenv
      g'_if destt cont regenv exp (AIfFEq rx ry) e1 e2
  AIfFLe x y e1 e2 -> do
      rx <- find x TFloat regenv
      ry <- find y TFloat regenv
      g'_if destt cont regenv exp (AIfFLe rx ry) e1 e2

  ACallCls x ys zs -> do
      rx <- find x TInt regenv
      g'_call destt cont regenv exp (ACallCls rx) ys zs

  ACallDir l ys zs -> do
      g'_call destt cont regenv exp (ACallDir l) ys zs

  ASave{} -> assert False undefined

g'_and_restore :: (Id,Type) -> Asm -> Map Id Id -> AExpr -> Caml (Asm, Map Id Id)
g'_and_restore destt cont regenv exp =
  g' destt cont regenv exp `catch` hdr
  where -- NoReg なら restore
    hdr (NoReg x t) = do
      liftIO $ putStrLn $ "restoring " ++ x
      g destt cont regenv (AsmLet (x, t) (ARestore x) (AsmAns exp))
    hdr e = throw e

g'_if :: (Id,Type) -> Asm -> Map Id Id -> AExpr
      -> (Asm -> Asm -> AExpr) -> Asm -> Asm -> Caml (Asm, Map Id Id)
g'_if destt cont regenv _exp constr e1 e2 = do
  (e1',regenv1) <- g destt cont regenv e1
  (e2',regenv2) <- g destt cont regenv e2
  let regenv' = foldl' func M.empty (fv cont) --共通部分
          where func env x =
                  if isReg x then env else
                  case (M.lookup x regenv1, M.lookup x regenv2) of
                    (Just r1, Just r2) | r1 /= r2  -> env
                    _ -> env
      f e x | x == fst destt || M.notMember x regenv || M.member x regenv' = return e
            | otherwise = seq' (ASave (lookupJust x regenv) x, e)
  e <- foldlM f (AsmAns $ constr e1' e2') (fv cont)
  return (e, regenv')

g'_call :: (Id,Type) -> Asm -> Map Id Id -> AExpr
        -> ([Id] -> [Id] -> AExpr) -> [Id] -> [Id] -> Caml (Asm, Map Id Id)
g'_call destt cont regenv _exp constr ys zs = do
  let f e x | x == fst destt || M.notMember x regenv = return e
            | otherwise = seq' (ASave (lookupJust x regenv) x, e)
  rys <- (mapM (\y -> find y TInt   regenv) ys)
  rzs <- (mapM (\z -> find z TFloat regenv) zs)
  e <- foldlM f (AsmAns $ constr rys rzs) (fv cont)
  return (e, M.empty)

h :: AFunDef -> Caml AFunDef
h (AFunDef (Label x) ys zs e t) = do
  let regenv = M.insert x regCl M.empty
      (_i,argRegs,regenv') = foldl' func (0,[],regenv) ys
          where func (i,argRegs_,regenv_) y =
                  let r = regs!i
                  in  (i+1, argRegs_++[r], M.insert y r regenv_)
      (_f,fargRegs,regenv'') = foldl' func (0,[],regenv') zs
          where func (d,fargRegs_,regenv_) z =
                  assert (not (isReg z)) $
                  let fr = fregs!d
                  in  (d+1, fargRegs_++[fr], M.insert z fr regenv_)
  a <- case t of
         TUnit -> genTmp TUnit
         TFloat -> return $ fregs!0
         _ -> return $ regs!0
  (e',_regenv''') <- g (a,t) (AsmAns (AMov a)) regenv'' e
  return $ AFunDef (Label x) argRegs fargRegs e' t

regAlloc :: AProg -> Caml AProg
regAlloc (AProg fdata fundefs e) = do
  liftIO $ putStrLn $
      "register allocation: may take some time " ++
      "(up to a few minutes, depending on the size of functions)"
  fundefs' <- mapM h fundefs
  tmp <- genTmp TUnit
  (e',_regenv) <- g (tmp,TUnit) (AsmAns ANop) M.empty e
  return $ AProg fdata fundefs' e'

----------
-- Util --
----------
lookupJust :: Ord k => k -> Map k a -> a
lookupJust x env = fromJust $ M.lookup x env


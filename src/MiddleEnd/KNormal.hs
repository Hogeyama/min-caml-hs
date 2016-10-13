{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module MiddleEnd.KNormal (
  KExpr(..),
  KFunDef(..),
  kNormalize,
  fv
) where

import Base
import FrontEnd.Syntax
import Control.Lens
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

-----------------------
-- KNormal.t = KExpr --
-----------------------
data KExpr = KUnit
           | KInt Int
           | KFloat Double
           | KNeg  Id
           | KAdd  Id Id
           | KSub  Id Id
           | KFNeg Id
           | KFAdd Id Id
           | KFSub Id Id
           | KFMul Id Id
           | KFDiv Id Id
           | KIfEq Id Id KExpr KExpr
           | KIfLe Id Id KExpr KExpr
           | KLet  (Id, Type) KExpr KExpr
           | KVar  Id
           | KLetRec KFunDef KExpr
           | KApp Id [Id]
           | KTuple [Id]
           | KLetTuple [(Id,Type)] Id KExpr
           | KGet Id Id
           | KPut Id Id Id
           | KExtArray Id
           | KExtFunApp Id [Id]
           deriving (Show, Eq)
data KFunDef = KFunDef { _kname ::  (Id,Type)
                       , _kargs :: [(Id,Type)]
                       , _kbody :: KExpr
                       }
              deriving (Show, Eq)
{-makeLenses ''KFunDef-}

fv :: KExpr -> Set Id
fv = \case
  KUnit -> S.empty
  KInt _ ->  S.empty
  KFloat _ -> S.empty
  KExtArray _ -> S.empty

  KNeg x -> S.singleton x
  KFNeg x -> S.singleton x

  KAdd  x y -> S.fromList [x ,y]
  KSub  x y -> S.fromList [x ,y]
  KFAdd x y -> S.fromList [x ,y]
  KFSub x y -> S.fromList [x ,y]
  KFMul x y -> S.fromList [x ,y]
  KFDiv x y -> S.fromList [x ,y]
  KGet  x y -> S.fromList [x ,y]

  KIfEq x y e1 e2 -> S.insert x (S.insert y (S.union (fv e1) (fv e2)))
  KIfLe x y e1 e2 -> S.insert x (S.insert y (S.union (fv e1) (fv e2)))

  KLet (x,_t) e1 e2 -> S.union (fv e1) (S.delete x (fv e2))

  KVar x -> S.singleton x

  KLetRec (KFunDef (x,_t) yts e1) e2 ->
    let zs = S.difference (fv e1) (S.fromList $ map fst yts)
    in  S.difference (S.union zs (fv e2)) (S.singleton x) -- deleteじゃダメなの？

  KApp x ys -> S.fromList (x:ys)

  KTuple xs       -> S.fromList xs
  KExtFunApp _ xs -> S.fromList xs

  KPut x y z -> S.fromList [x,y,z]

  KLetTuple xts y e -> S.insert y (S.difference (fv e) (S.fromList $ map fst xts))


insertLet :: Caml (KExpr,Type) -> (Id -> Caml (KExpr,Type)) -> Caml (KExpr, Type)
insertLet m k = m >>= \case
  (KVar x, _t) -> k x
  (e,      t) -> do
    x <- genTmp t
    (e',t') <- k x
    return (KLet (x,t) e e', t')

insertLetWithTy :: Caml (KExpr,Type) -> (Id -> Type -> Caml (KExpr,Type)) -> Caml (KExpr, Type)
insertLetWithTy m k = m >>= \case
  (KVar x, t) -> k x t
  (e,      t) -> do
    x <- genTmp t
    (e',t') <- k x t
    return (KLet (x,t) e e', t')

g :: Map Id Type -> Expr -> Caml (KExpr, Type)
g env e = case e of
  EUnit -> return (KUnit, TUnit)
  EBool True  -> return (KInt 1,TInt)
  EBool False -> return (KInt 0,TInt)
  EInt i -> return (KInt i, TInt)
  EFloat d -> return (KFloat d, TFloat)

  ENot e' ->
      g env (EIf e' (EBool False) (EBool True))

  ENeg e' ->
      insertLet (g env e') $ \x ->
      return (KNeg x, TInt)

  EAdd e1 e2 ->
      insertLet (g env e1) $ \x ->
      insertLet (g env e2) $ \y ->
      return (KAdd x y, TInt)
  ESub e1 e2 ->
      insertLet (g env e1) $ \x ->
      insertLet (g env e2) $ \y ->
      return (KSub x y, TInt)

  EFNeg e' ->
      insertLet (g env e') $ \x ->
      return (KFNeg x, TFloat)

  EFAdd e1 e2 ->
      insertLet (g env e1) $ \x ->
      insertLet (g env e2) $ \y ->
      return (KFAdd x y, TFloat)
  EFSub e1 e2 ->
      insertLet (g env e1) $ \x ->
      insertLet (g env e2) $ \y ->
      return (KFSub x y, TFloat)
  EFMul e1 e2 ->
      insertLet (g env e1) $ \x ->
      insertLet (g env e2) $ \y ->
      return (KFMul x y, TFloat)
  EFDiv e1 e2 ->
      insertLet (g env e1) $ \x ->
      insertLet (g env e2) $ \y ->
      return (KFDiv x y, TFloat)

  EEq _e1 _e2 ->
      g env (EIf e (EBool True) (EBool False))
  ELe _e1 _e2 ->
      g env (EIf e (EBool True) (EBool False))

  EIf (ENot e1) e2 e3 ->
      g env (EIf e1 e3 e2)

  EIf (EEq e1 e2) e3 e4 ->
      insertLet (g env e1) $ \x ->
      insertLet (g env e2) $ \y -> do
        (e3', t3) <- g env e3
        (e4',_t4) <- g env e4
        return (KIfEq x y e3' e4', t3)
  EIf (ELe e1 e2) e3 e4 ->
      insertLet (g env e1) $ \x ->
      insertLet (g env e2) $ \y -> do
        (e3', t3) <- g env e3
        (e4',_t4) <- g env e4
        return (KIfLe x y e3' e4', t3)

  EIf e1 e2 e3 ->
      g env (EIf (EEq e1 (EBool False)) e3 e2)

  ELet (x,t) e1 e2 -> do
      (e1',_t1) <- g env e1
      (e2', t2) <- g (M.insert x t env) e2
      return (KLet (x,t) e1' e2', t2)

  EVar x ->
      case M.lookup x env of
        Just t  -> return (KVar x, t)
        Nothing -> uses extTyEnv (M.lookup x) >>= \case
          Just t@(TArray _) -> return (KExtArray x, t)
          _ -> throw $ Failure ("external variable " ++ x ++" does not have an array type")

  ELetRec (EFunDef (x,t) yts e1) e2 -> do
      let env' = M.insert x t env
      (e2', t2) <- g env' e2
      (e1',_t1) <- g (M.union (M.fromList yts) env') e1
      return (KLetRec (KFunDef (x,t) yts e1') e2', t2)

  EApp (EVar f) e2s
    | M.notMember f env -> uses extTyEnv (M.lookup f) >>= \case
        Just (TFun _ t) ->
          let bind xs []       = return (KExtFunApp f xs, t)
              bind xs (e2:e2s') = insertLet (g env e2) $ \x -> bind (xs++[x]) e2s'
          in  bind [] e2s
        _ -> error "KNormal:Aiee!!"

  EApp e1 e2s ->
      insertLetWithTy (g env e1) $ \f (TFun _ t) ->
        let bind xs []       = return (KApp f xs, t)
            bind xs (e2:e2s') = insertLet (g env e2) $ \x -> bind (xs++[x]) e2s'
        in  bind [] e2s

  ETuple es ->
      let bind xs ts []     = return (KTuple xs, TTuple ts)
          bind xs ts (e':es') = insertLetWithTy (g env e') $ \x t -> bind (xs++[x]) (ts++[t]) es'
      in  bind [] [] es

  ELetTuple xts e1 e2 ->
      insertLet (g env e1) $ \y -> do
        (e2',t2) <- g (M.union (M.fromList xts) env) e2
        return (KLetTuple xts y e2', t2)

  EArray e1 e2 ->
      insertLet       (g env e1) $ \x    ->
      insertLetWithTy (g env e2) $ \y t2 ->
        let l = case t2 of
                  TFloat -> "create_float_array"
                  _ -> "create_array"
        in  return (KExtFunApp l [x,y], TArray t2)

  EGet e1 e2 ->
    insertLetWithTy (g env e1) $ \x (TArray t) ->
    insertLet       (g env e2) $ \y -> return (KGet x y, t)

  EPut e1 e2 e3 ->
    insertLet (g env e1) $ \x ->
    insertLet (g env e2) $ \y ->
    insertLet (g env e3) $ \z ->
    return (KPut x y z, TUnit)

kNormalize :: Expr -> Caml KExpr
kNormalize e = fst <$> g M.empty e



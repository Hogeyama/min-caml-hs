{-# LANGUAGE LambdaCase #-}

module Beta where

import KNormal
import AllTypes
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

beta :: KExpr -> Caml KExpr
beta = g M.empty

find :: Id -> Map Id Id -> Id
find x env = fromMaybe x (M.lookup x env)

g :: Map Id Id -> KExpr -> Caml KExpr
g env e = case e of
  KUnit    -> return e
  KInt{}   -> return e
  KFloat{} -> return e

  KVar  x     -> return $ KVar      $ find x env
  KNeg  x     -> return $ KNeg      $ find x env
  KFNeg x     -> return $ KFNeg     $ find x env

  KAdd  x y -> return $ KAdd  (find x env) (find y env)
  KSub  x y -> return $ KSub  (find x env) (find y env)
  KFAdd x y -> return $ KFAdd (find x env) (find y env)
  KFSub x y -> return $ KFSub (find x env) (find y env)
  KFMul x y -> return $ KFMul (find x env) (find y env)
  KFDiv x y -> return $ KFDiv (find x env) (find y env)
  KGet  x y -> return $ KGet  (find x env) (find y env)
  KPut  x y z -> return $ KPut (find x env) (find y env) (find z env)

  KIfEq x y e1 e2 -> KIfEq (find x env) (find y env) <$> g env e1 <*> g env e2
  KIfLe x y e1 e2 -> KIfLe (find x env) (find y env) <$> g env e1 <*> g env e2

  KLet (x,t) e1 e2 ->
      g env e1 >>= \case
        KVar y -> do
          liftIO $ putStrLn $ "beta-reducing " ++ x ++ " = " ++ y
          g (M.insert x y env) e2
        e1' -> do
          e2' <- g env e2
          return $ KLet (x,t) e1' e2'

  KLetRec (KFunDef (x,t) yts e1) e2 -> do
      e1' <- g env e1
      e2' <- g env e2
      return $ KLetRec (KFunDef (x,t) yts e1') e2'

  KLetTuple xts y e' ->
      KLetTuple xts (find y env) <$> g env e'

  KApp       x ys -> return $ KApp (find x env) (map (`find` env) ys)
  KExtFunApp x ys -> return $ KExtFunApp x      (map (`find` env) ys)

  KTuple xs -> return $ KTuple (map (`find` env) xs)
  KExtArray x -> return $ KExtArray x


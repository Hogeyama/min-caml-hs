
module Alpha where

import AllTypes
import Id
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

alpha :: KExpr -> Caml KExpr
alpha = g M.empty

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

  KLet (x,t) e1 e2 -> do
      x' <- genId x
      e1' <- g env e1
      e2' <- g (M.insert x x' env) e2
      return $ KLet (x',t) e1' e2'

  KLetRec (KFunDef (x,t) yts e1) e2 -> do
      x' <- genId x
      let (ys,ts) = unzip yts
      ys' <- mapM genId ys
      let env'  = M.insert x x' env
          env'' = M.union (M.fromList (zip ys ys')) env'
      e1' <- g env'' e1
      e2' <- g env'  e2
      return $ KLetRec (KFunDef (x',t) (zip ys' ts) e1') e2'

  KLetTuple xts y e' -> do
      let (xs,ts) = unzip xts
      xs' <- mapM genId xs
      let env' = M.union (M.fromList (zip xs xs')) env
      KLetTuple (zip xs' ts) (find y env) <$> g env' e'


  KApp x ys -> return $ KApp (find x env) (map (`find` env) ys)
  KExtArray x -> return $ KExtArray x
  KExtFunApp x ys -> return $ KExtFunApp x (map (`find` env) ys)
  KTuple xs -> return $ KTuple (map (`find` env) xs)



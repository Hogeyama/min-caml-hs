{-# LANGUAGE LambdaCase #-}

module Inline where

import KNormal
import qualified Alpha
import AllTypes
import Data.Map (Map)
import qualified Data.Map as M
import Control.Lens

inline :: KExpr -> Caml KExpr
inline = g M.empty

size :: KExpr -> Int
size = \case
  KIfEq _ _ e1 e2             -> 1 + size e1 + size e2
  KIfLe _ _ e1 e2             -> 1 + size e1 + size e2
  KLet _ e1 e2                -> 1 + size e1 + size e2
  KLetRec (KFunDef _ _ e1) e2 -> 1 + size e1 + size e2
  KLetTuple _ _ e             -> 1 + size e
  _ -> 1

g :: Map Id ([(Id,Type)], KExpr) -> KExpr -> Caml KExpr
g env e = case e of
  KIfEq x y e1 e2 -> KIfEq x y <$> g env e1 <*> g env e2
  KIfLe x y e1 e2 -> KIfLe x y <$> g env e1 <*> g env e2

  KLet xt e1 e2 -> KLet xt <$> g env e1 <*> g env e2
  KLetRec (KFunDef (x,t) yts e1) e2 -> do
    threshold <- use threshold
    let env' = if size e1 > threshold then env else M.insert x (yts,e1) env
    e1' <- g env' e1
    e2' <- g env' e2
    return $ KLetRec (KFunDef (x,t) yts e1') e2'
  KApp x ys ->
    case M.lookup x env of
      Just (zts,e) -> do
        liftIO $ putStrLn $ "inlining " ++ x
        let env' = M.fromList (zip (map fst zts) ys)
        Alpha.g env' e
      _ -> return e
  KLetTuple xts y e -> KLetTuple xts y <$> g env e
  e -> return e


{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Elim where

import Id
import KNormal
import CamlMonad
import qualified Data.Set as S

hasSubEffect :: KExpr -> Bool
hasSubEffect = \case
  KLet _ e1 e2    -> hasSubEffect e1 || hasSubEffect e2
  KIfEq _ _ e1 e2 -> hasSubEffect e1 || hasSubEffect e2
  KIfLe _ _ e1 e2 -> hasSubEffect e1 || hasSubEffect e2
  KLetRec _ e     -> hasSubEffect e
  KLetTuple _ _ e -> hasSubEffect e
  KApp{} -> True
  KPut{} -> True
  KExtFunApp{} -> True
  _ -> False

elim :: KExpr -> Caml KExpr
elim = \case
  KIfEq x y e1 e2 -> KIfEq x y <$> elim e1 <*> elim e2
  KIfLe x y e1 e2 -> KIfLe x y <$> elim e1 <*> elim e2
  KLet (x,t) e1 e2 -> do
    e1' <- elim e1
    e2' <- elim e2
    if hasSubEffect e1' || S.member x (fv e2')
       then return $ KLet (x,t) e1' e2'
       else liftIO (putStrLn $ "eliminating variable " ++ x) >> return e2'
  KLetRec (KFunDef (x,t) yts e1) e2 -> do
    e1' <- elim e1
    e2' <- elim e2
    if S.member x (fv e2')
       then return $ KLetRec (KFunDef (x,t) yts e1') e2'
       else liftIO (putStrLn $ "eliminating variable " ++ x) >> return e2'
  KLetTuple xts y e -> do
    e' <- elim e
    let xs = map fst xts
        live x = S.member x (fv e')
    if any live xs
       then return $ KLetTuple xts y e'
       else liftIO (putStrLn $ "eliminating variable " ++ ppList xs) >> return e'
  e -> return e



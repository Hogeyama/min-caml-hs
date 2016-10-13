
module MiddleEnd.ConstFold where

import Base
import MiddleEnd.KNormal

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import           Data.List (foldl')

constFold :: KExpr -> Caml KExpr
constFold = return . g M.empty

memberI :: Id -> Map Id KExpr -> Bool
memberI x env = case M.lookup x env of
                  Just (KInt _) -> True
                  _ -> False
memberF :: Id -> Map Id KExpr -> Bool
memberF x env = case M.lookup x env of
                  Just (KFloat _) -> True
                  _ -> False
memberT :: Id -> Map Id KExpr -> Bool
memberT x env = case M.lookup x env of
                  Just (KTuple _) -> True
                  _ -> False
findI :: Id -> Map Id KExpr -> Int
findI x env = case fromJust $ M.lookup x env of KInt i -> i; _ -> error "findI"
findF :: Id -> Map Id KExpr -> Double
findF x env = case fromJust $ M.lookup x env of KFloat f -> f; _ -> error "findF"
findT :: Id -> Map Id KExpr -> [Id]
findT x env = case fromJust $ M.lookup x env of KTuple ys -> ys; _ -> error "findT"

g :: Map Id KExpr -> KExpr -> KExpr
g env e = case e of
  KVar x
    | memberI x env -> KInt   $ findI x env
    | memberF x env -> KFloat $ findF x env
    | memberT x env -> KTuple $ findT x env
                  {-下2つは何故かコメントアウトされていた-}
  KNeg x
    | memberI x env -> KInt $ - findI x env
  KFNeg x
    | memberF x env -> KFloat $ - findF x env

  KAdd x y
    | memberI x env && memberI y env -> KInt $ findI x env + findI y env
  KSub x y
    | memberI x env && memberI y env -> KInt $ findI x env - findI y env

  KFAdd x y
    | memberF x env && memberF y env -> KFloat $ findF x env + findF y env
  KFSub x y
    | memberF x env && memberF y env -> KFloat $ findF x env - findF y env
  KFMul x y
    | memberF x env && memberF y env -> KFloat $ findF x env * findF y env
  KFDiv x y
    | memberF x env && memberF y env -> KFloat $ findF x env / findF y env

  KIfEq x y e1 e2
    | memberI x env && memberI y env && findI x env == findI y env -> g env e1
    | memberI x env && memberI y env                               -> g env e2
    | memberF x env && memberF y env && findF x env == findF y env -> g env e1
    | memberF x env && memberF y env                               -> g env e2
    | otherwise -> KIfEq x y (g env e1) (g env e2)
  KIfLe x y e1 e2
    | memberI x env && memberI y env && findI x env <= findI y env -> g env e1
    | memberI x env && memberI y env                               -> g env e2
    | memberF x env && memberF y env && findF x env <= findF y env -> g env e1
    | memberF x env && memberF y env                               -> g env e2
    | otherwise -> KIfLe x y (g env e1) (g env e2)

  KLet (x,t) e1 e2 ->
      let e1' = g env e1
          e2' = g (M.insert x e1' env) e2
      in  KLet (x,t) e1' e2'

  KLetRec (KFunDef xt yts e1) e2 ->
      KLetRec (KFunDef xt yts (g env e1)) (g env e2)

  KLetTuple xts y e'
    | memberT y env -> let f e'' (xt,z) = KLet xt (KVar z) e''
                       in  foldl' f (g env e') (zip xts (findT y env))
    | otherwise -> KLetTuple xts y (g env e')

  _ -> e


{-# LANGUAGE LambdaCase #-}

module Virtual where

import Id
import Asm
import AllTypes
import qualified Closure as C

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Lens
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Foldable (foldlM)

virtualCode :: CProg -> Caml AProg
virtualCode (CProg fundefs e) = do
  virtualData .= []
  fundefs' <- mapM h fundefs
  e' <- g M.empty e
  fdata <- use virtualData
  return $ AProg fdata fundefs' e'

h :: CFunDef -> Caml AFunDef
h (CFunDef (Label x,t) yts zts e) = do
  let (int, float) = separate yts
  (_offset, load) <- do
      e' <- g (M.insert x t (insertList yts $ insertList zts M.empty)) e
      let f1 z offset load = fLetD (z, ALdDF x (C offset) 1, load)
          f2 z t' offset load = AsmLet (z,t') (ALd x (C offset) 1) load
      return $ expand zts (4,e') f1 f2
  case t of
    TFun _ t2 -> return $ AFunDef (Label x) int float load t2
    _ -> error "Virtual.h"

separate :: [(Id, Type)] -> ([Id], [Id])
separate xts =
  classify xts ([],[])
    (\(int,float) x   -> (int, float ++ [x]))
    (\(int,float) x _ -> (int ++ [x], float))

--classify :: [(a, Type)] -> b
--         -> (b -> a -> Caml b)
--         -> (b -> a -> Type -> Caml b)
--         -> Caml b
classify :: [(a, Type)] -> b -> (b -> a -> b) -> (b -> a -> Type -> b) -> b
classify xts ini addf addi = foldl' func ini xts
  where func acc (x,t) = case t of
          TUnit   -> acc
          TFloat -> addf acc x
          _      -> addi acc x t

expand :: [(a, Type)]
       -> (Int, t)
       -> (a -> Int -> t -> t)
       -> (a -> Type -> Int -> t -> t)
       -> (Int, t)
expand xts ini addf addi =
  classify xts ini
    (\(offset, acc) x ->
        let offset' = align offset in
        (offset' + 8, addf x offset' acc))
    (\(offset, acc) x t ->
      (offset + 4, addi x t offset acc))

classifyM :: [(a, Type)] -> b
          -> (b -> a -> Caml b)
          -> (b -> a -> Type -> Caml b)
          -> Caml b
classifyM xts ini addf addi = foldlM func ini xts
  where func acc (x,t) = case t of
          TUnit  -> return acc
          TFloat -> addf acc x
          _      -> addi acc x t

expandM :: [(a, Type)]
        -> (Int, t)
        -> (a -> Int -> t -> Caml t)
        -> (a -> Type -> Int -> t -> Caml t)
        -> Caml (Int, t)
expandM xts ini addf addi =
  classifyM xts ini
    (\(offset, acc) x -> do
        let offset' = align offset
        z <- addf x offset acc
        return (offset' + 8, z))
    (\(offset, acc) x t -> do
        z <- addi x t offset acc
        return (offset + 4, z))

----------
-- Util --
----------
insertList :: Ord key => [(key,a)] -> Map key a -> Map key a
insertList xts m = M.union (M.fromList xts) m

-- 通常のlookupと比べて a,b が逆
lookupRev :: (Eq a) => a -> [(b,a)] -> Maybe b
lookupRev i = let f (p,q) = (q,p) in lookup i . map f
----------
-- Util --
----------


g :: Map Id Type -> CExpr -> Caml Asm
g env = \case
  CUnit -> return $ AsmAns ANop
  CInt i -> return $ AsmAns $ ASet i
  CFloat d -> do
    fdata <- use virtualData
    l <- case lookupRev d fdata of
           Just l -> return l
           _ -> do l <- Label <$> genId "l"
                   virtualData .= (l,d):fdata
                   return l
    x <- genId "l"
    return $ AsmLet (x,TInt) (ASetL l) (AsmAns (ALdDF x (C 0) 1))

  CNeg x -> return $ AsmAns $ ANeg x
  CAdd x y -> return $ AsmAns $ AAdd x (V y)
  CSub x y -> return $ AsmAns $ ASub x (V y)

  CFNeg x -> return $ AsmAns $ AFNegD x
  CFAdd x y -> return $ AsmAns $ AFAddD x y
  CFSub x y -> return $ AsmAns $ AFSubD x y
  CFMul x y -> return $ AsmAns $ AFMulD x y
  CFDiv x y -> return $ AsmAns $ AFDivD x y

  CIfEq x y e1 e2 -> case M.lookup x env of
    Just TBool ->  AsmAns <$> (AIfEq  x (V y) <$> g env e1 <*> g env e2)
    Just TInt  ->  AsmAns <$> (AIfEq  x (V y) <$> g env e1 <*> g env e2)
    Just TFloat -> AsmAns <$> (AIfFEq x    y  <$> g env e1 <*> g env e2)
    _ -> throw $ Failure "equality supported only for bool, int, and float"
  CIfLe x y e1 e2 -> case M.lookup x env of
    Just TBool ->  AsmAns <$> (AIfLe  x (V y) <$> g env e1 <*> g env e2)
    Just TInt  ->  AsmAns <$> (AIfLe  x (V y) <$> g env e1 <*> g env e2)
    Just TFloat -> AsmAns <$> (AIfFLe x    y  <$> g env e1 <*> g env e2)
    _ -> throw $ Failure "equality supported only for bool, int, and float"

  CLet (x,t1) e1 e2 -> do
    e1' <- g env e1
    e2' <- g (M.insert x t1 env) e2
    return $ concat' e1' (x,t1) e2'

  CVar x -> case M.lookup x env of
    Just TUnit -> return $ AsmAns ANop
    Just TFloat -> return $ AsmAns $ AFMovD x
    _ -> return $ AsmAns $ AMov x

  CMakeCls (x,t) (Closure l ys) e2 -> do
    e2' <- g (M.insert x t env) e2
    let ts = [ fromJust (M.lookup y env) | y <- ys ]
    (offset,storeFv) <-
        let addf y   offset storeFv = seq' (AStDF y x (C offset) 1, storeFv)
            addi y _ offset storeFv = seq' (ASt   y x (C offset) 1, storeFv)
        in  expandM (zip ys ts) (4,e2') addf addi
    e1 <- do
        e2'' <- do
            z <- genId "l"
            AsmLet (z,TInt) (ASetL l) <$> seq' (ASt z x (C 0) 1, storeFv)
        return $ AsmLet (regHp, TInt) (AAdd regHp (C $ align offset)) e2''
    return $ AsmLet (x,t) (AMov regHp) e1

  CAppCls x ys ->
    let (int,float) = separate [(y, fromJust (M.lookup y env)) | y <- ys]
    in  return $ AsmAns $ ACallCls x int float
  CAppDir (Label x) ys ->
    let (int,float) = separate [(y, fromJust (M.lookup y env)) | y <- ys]
    in  return $ AsmAns $ ACallDir (Label x) int float

  CTuple xs -> do
    y <- genId "t"
    let ts = [fromJust (M.lookup x env) | x <- xs]
    (offset,store) <-
        let addi x   offset store = seq' (AStDF x y (C offset) 1, store)
            addf x _ offset store = seq' (ASt   x y (C offset) 1, store)
        in expandM (zip xs ts) (0,AsmAns (AMov y)) addi addf
    return $ AsmLet (y,TTuple ts) (AMov regHp)
                (AsmLet (regHp,TInt) (AAdd regHp (C $ align offset)) store)

  CLetTuple xts y e2 -> do
    let s = C.fv e2
    (_offset, load) <- do
        let addi x offset load
                | S.member x s = fLetD (x, ALdDF y (C offset) 1, load)
                | otherwise    = load
            addf x t offset load
                | S.member x s = AsmLet (x,t) (ALd y (C offset) 1) load
                | otherwise    = load
        e2' <- g (insertList xts env) e2
        return $ expand xts (0,e2') addi addf
    return load

  CGet x y -> case M.lookup x env of
    Just (TArray TUnit)  -> return $ AsmAns ANop
    Just (TArray TFloat) -> return $ AsmAns (ALdDF x (V y) 8)
    Just (TArray _)      -> return $ AsmAns (ALd   x (V y) 4)
    _ -> error "Virtual.g CGet"

  CPut x y z -> case M.lookup x env of
    Just (TArray TUnit)  -> return $ AsmAns ANop
    Just (TArray TFloat) -> return $ AsmAns (AStDF z x (V y) 8)
    Just (TArray _)      -> return $ AsmAns (ASt   z x (V y) 4)
    _ -> error "Virtual.g CPut"

  CExtArray (Label x) -> return $ AsmAns $ ASetL $ Label $ "min_caml_" ++ x


{-{{{
{{{
let rec g env = function (* 式の仮想マシンコード生成 (caml2html: virtual_g) *)
  | Closure.Unit -> Ans(Nop)
  | Closure.Int(i) -> Ans(Set(i))
  | Closure.Float(d) ->
      let l =
        try
          (* すでに定数テーブルにあったら再利用 *)
          let (l, _) = List.find (fun (_, d') -> d = d') !data in
          l
        with Not_found ->
          let l = Id.L(Id.genid "l") in
          data := (l, d) :: !data;
          l in
      let x = Id.genid "l" in
      Let((x, Type.Int), SetL(l), Ans(LdDF(x, C(0), 1)))
  | Closure.MakeCls((x, t), { Closure.entry = l; Closure.actual_fv = ys }, e2) -> (* クロージャの生成 (caml2html: virtual_makecls) *)
      (* Closureのアドレスをセットしてから、自由変数の値をストア *)
      let e2' = g (M.add x t env) e2 in
      let offset, store_fv =
        expand
          (List.map (fun y -> (y, M.find y env)) ys)
          (4, e2')
          (fun y   offset store_fv -> seq(StDF(y, x, C(offset), 1), store_fv))
          (fun y _ offset store_fv -> seq(St  (y, x, C(offset), 1), store_fv)) in
      Let((x, t),
          Mov(reg_hp),
          Let((reg_hp, Type.Int),
              Add(reg_hp, C(align offset)),
              let z = Id.genid "l" in
                Let((z, Type.Int),
                    SetL(l),
                    seq(St(z, x, C(0), 1), store_fv))))
  | Closure.AppCls(x, ys) ->
      let (int, float) = separate (List.map (fun y -> (y, M.find y env)) ys) in
      Ans(CallCls(x, int, float))
  | Closure.AppDir(Id.L(x), ys) ->
      let (int, float) = separate (List.map (fun y -> (y, M.find y env)) ys) in
      Ans(CallDir(Id.L(x), int, float))
  | Closure.Tuple(xs) -> (* 組の生成 (caml2html: virtual_tuple) *)
      let y = Id.genid "t" in
      let (offset, store) =
        expand
          (List.map (fun x -> (x, M.find x env)) xs)
          (0, Ans(Mov(y)))
          (fun x offset store -> seq(StDF(x, y, C(offset), 1), store))
          (fun x _ offset store -> seq(St(x, y, C(offset), 1), store)) in
      Let((y, Type.Tuple(List.map (fun x -> M.find x env) xs)), Mov(reg_hp),
          Let((reg_hp, Type.Int), Add(reg_hp, C(align offset)),
              store))
}}}
  | Closure.ExtArray(Id.L(x)) -> Ans(SetL(Id.L("min_caml_" ^ x)))
  }}}-}

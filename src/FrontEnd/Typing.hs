{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module FrontEnd.Typing where

import Base
import FrontEnd.Syntax

import           Control.Monad (zipWithM_, join)
import qualified Data.Map as M
import           Control.Lens

-- main function
typing :: Expr -> Caml Expr
typing e = do
  extTyEnv .= M.empty
  t <- infer M.empty e
  unify TUnit t `catch`
    (\Unify{} -> throw $ Failure "top level does not have type unit")
  extTyEnv <~ join (uses extTyEnv (mapM derefType))
  derefExpr e

-- zonkType
derefType :: Type -> Caml Type
derefType = \case
  TFun t1s t2 -> TFun <$> mapM derefType t1s <*> derefType t2
  TTuple ts -> TTuple <$> mapM derefType ts
  TArray t -> TArray <$> derefType t
  TVar tv -> readType tv >>= \case
                Nothing -> do
                  liftIO $ putStrLn "uninstantiated type variable detected; assuming int@."
                  writeType tv TInt
                  return TInt
                Just t -> do
                  t' <- derefType t
                  writeType tv t'
                  return t'
  t -> return t

derefIdType :: (Id, Type) -> Caml (Id, Type)
derefIdType (x,t) = (x,) <$> derefType t

derefExpr :: Expr -> Caml Expr
derefExpr = \case
  ENot e        -> ENot   <$> derefExpr e
  ENeg e        -> ENeg   <$> derefExpr e
  EAdd e1 e2    -> EAdd   <$> derefExpr e1 <*> derefExpr e2
  ESub e1 e2    -> ESub   <$> derefExpr e1 <*> derefExpr e2
  EEq  e1 e2    -> EEq    <$> derefExpr e1 <*> derefExpr e2
  ELe  e1 e2    -> ELe    <$> derefExpr e1 <*> derefExpr e2
  EFNeg e       -> EFNeg  <$> derefExpr e
  EFAdd e1 e2   -> EFAdd  <$> derefExpr e1 <*> derefExpr e2
  EFSub e1 e2   -> EFSub  <$> derefExpr e1 <*> derefExpr e2
  EFMul e1 e2   -> EFMul  <$> derefExpr e1 <*> derefExpr e2
  EFDiv e1 e2   -> EFDiv  <$> derefExpr e1 <*> derefExpr e2
  EIf e1 e2 e3  -> EIf    <$> derefExpr e1 <*> derefExpr e2 <*> derefExpr e3
  EApp e es     -> EApp   <$> derefExpr e <*> mapM derefExpr es
  EArray e1 e2  -> EArray <$> derefExpr e1 <*> derefExpr e2
  EGet e1 e2    -> EGet   <$> derefExpr e1 <*> derefExpr e2
  EPut e1 e2 e3 -> EPut   <$> derefExpr e1 <*> derefExpr e2 <*> derefExpr e3
  ETuple es     -> ETuple <$> mapM derefExpr es
  ELet xt e1 e2       -> ELet      <$> derefIdType xt       <*> derefExpr e1 <*> derefExpr e2
  ELetTuple xts e1 e2 -> ELetTuple <$> mapM derefIdType xts <*> derefExpr e1 <*> derefExpr e2
  ELetRec (EFunDef xt args e1) e2
        -> ELetRec <$> (EFunDef <$> derefIdType xt <*> mapM derefIdType args <*> derefExpr e1)
                   <*> derefExpr e2
  e -> return e

occur :: TV -> Type -> Caml Bool
occur tv1 = \case
  TFun t2s t2 -> anyM (occur tv1) (t2:t2s)
  TTuple t2s  -> anyM (occur tv1) t2s
  TArray t2 -> occur tv1 t2
  TVar tv2
    | tv1==tv2  -> return True
    | otherwise -> readType tv2 >>= \case
        Nothing -> return False
        Just t2 -> occur tv1 t2
  _ -> return False

unify :: Type -> Type -> Caml ()
unify t1 t2 = case (t1,t2) of
  (TUnit, TUnit ) -> return ()
  (TInt,  TInt  ) -> return ()
  (TBool, TBool ) -> return ()
  (TFloat,TFloat) -> return ()

  (TFun t1s t1', TFun t2s t2')
    | length t1s == length t2s -> zipWithM_ unify (t1':t1s) (t2':t2s)
    | otherwise -> throw $ Unify t1 t2
  (TTuple t1s, TTuple t2s)
    | length t1s == length t2s -> zipWithM_ unify t1s t2s
    | otherwise -> throw $ Unify t1 t2
  (TArray t1', TArray t2') -> unify t1' t2'
  (TVar ref1, TVar ref2)
    | ref1 == ref2 -> return ()
    | otherwise -> (,) <$> readType ref1 <*> readType ref2 >>= \case
          (Just t1', _) -> unify t1' t2
          (_, Just t2') -> unify t1  t2'
          _             -> writeType ref1 t2
  (TVar ref1, _) -> readType ref1 >>= \case
      Nothing -> ifM (occur ref1 t2)
                     (throw $ Unify t1 t2)
                     (writeType ref1 t2)
      Just t1' -> unify t1' t2
  (_, TVar ref2) -> readType ref2 >>= \case
      Nothing -> ifM (occur ref2 t1)
                     (throw $ Unify t1 t2)
                     (writeType ref2 t1)
      Just t2' -> unify t1 t2'
  _ -> throw $ Unify t1 t2

unifyM1 :: Type -> Caml Type -> Caml ()
unifyM1 t1 m2 = do
  t2 <- m2
  unify t1 t2

unifyM2 :: Caml Type -> Caml Type -> Caml ()
unifyM2 m1 m2 = do
  t1 <- m1
  t2 <- m2
  unify t1 t2

infer :: TyEnv -> Expr -> Caml Type
infer env e =
  case e of
    EUnit -> return TUnit
    EBool _ -> return TBool
    EInt _ -> return TInt
    EFloat _ -> return TFloat

    ENot e' -> do
        unifyM1 TBool (infer env e')
        return TBool

    ENeg e' -> do
        unifyM1 TInt (infer env e')
        return TInt
    EFNeg e' -> do
        unifyM1 TFloat (infer env e')
        return TFloat

    EAdd e1 e2 -> do
        unifyM1 TInt (infer env e1)
        unifyM1 TInt (infer env e2)
        return TInt
    ESub e1 e2 -> do
        unifyM1 TInt (infer env e1)
        unifyM1 TInt (infer env e2)
        return TInt

    EFAdd e1 e2 -> do
        unifyM1 TFloat (infer env e1)
        unifyM1 TFloat (infer env e2)
        return TFloat
    EFSub e1 e2 -> do
        unifyM1 TFloat (infer env e1)
        unifyM1 TFloat (infer env e2)
        return TFloat
    EFMul e1 e2 -> do
        unifyM1 TFloat (infer env e1)
        unifyM1 TFloat (infer env e2)
        return TFloat
    EFDiv e1 e2 -> do
        unifyM1 TFloat (infer env e1)
        unifyM1 TFloat (infer env e2)
        return TFloat

    EEq e1 e2 -> do
        unifyM2 (infer env e1) (infer env e2)
        return TBool
    ELe e1 e2 -> do
        unifyM2 (infer env e1) (infer env e2)
        return TBool

    EIf e1 e2 e3 -> do
        unifyM1 TBool (infer env e1)
        t2 <- infer env e2
        t3 <- infer env e3
        unify t2 t3
        return t2

    ELet (x,t) e1 e2 -> do
        unifyM1 t (infer env e1)
        infer (M.insert x t env) e2

    EVar x -> -- env -> S.extTyEnv と探し, なかったらS.extTyEnvに追加
      case M.lookup x env of
        Just t  -> return t
        Nothing -> uses extTyEnv (M.lookup x) >>= \case
          Just t -> return t
          Nothing -> do
            liftIO $ putStrLn $ "free variable " ++ x ++ " assumed as external@."
            t <- genType
            extTyEnv %= M.insert x t
            return t

    ELetRec (EFunDef (x,t) yts e1) e2 -> do
      let env' = M.insert x t env
      let argtys = map snd yts
      t' <- infer (M.union (M.fromList yts) env') e1
      unify t (TFun argtys t')
      infer env' e2

    EApp e' es' -> do
      tret <- genType
      argtys <- mapM (infer env) es'
      unifyM1 (TFun argtys tret) (infer env e')
      return tret

    ETuple es -> TTuple <$> mapM (infer env) es

    ELetTuple xts e1 e2 -> do
      unifyM1 (TTuple $ map snd xts) (infer env e1)
      infer (M.union (M.fromList xts) env) e2

    EArray e1 e2 -> do -- must be a primitive for "polymorphic" typing #とは
      unifyM1 TInt (infer env e1)
      TArray <$> infer env e2

    EGet e1 e2 -> do
      t <- genType
      unifyM1 (TArray t) (infer env e1)
      unifyM1 TInt       (infer env e2)
      return t

    EPut e1 e2 e3 -> do
      t <- infer env e3
      unifyM1 (TArray t) (infer env e1)
      unifyM1 TInt       (infer env e2)
      return TUnit

  `catch`
    \err -> case err of
      Unify t1 t2 -> do
        e' <- derefExpr e
        t1' <- derefType t1
        t2' <- derefType t2
        throw $ Typing e' t1' t2'
      _ -> throw err


----------
-- Util --
----------

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b x y = b >>= \case True -> x; False -> y

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _m []     = return False
anyM  m (x:xs) = ifM (m x) (return True) (anyM m xs)

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _m []     = return True
allM  m (x:xs) = ifM (m x) (allM m xs) (return False)


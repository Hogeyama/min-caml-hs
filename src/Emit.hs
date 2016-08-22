{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module Emit where

import Id
import Asm
import Type
import AllTypes

import Data.Word (Word32)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Control.Lens
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Foldable (foldlM)
import Control.Exception.Base (assert)
import qualified Data.Foldable as F
import Control.Monad (when, forM_)
import Data.List (partition)
import System.IO (Handle, hPutStr, hPutStrLn)
import Text.Printf

-- ghc-modが動かなくなるので書いている間はコメントアウト
foreign import ccall "gethi" gethi :: Double -> Word32
foreign import ccall "getlo" getlo :: Double -> Word32
{-gethi = undefined :: Double -> Word32-}
{-getlo = undefined :: Double -> Word32-}

save :: Id -> Caml ()
save x = do
  stackSet %= S.insert x
  stackMap %= \l -> if x `elem` l then l else l++[x]

savef :: Id -> Caml ()
savef x = do
  stackSet %= S.insert x
  smap <- use stackMap
  when (x `notElem` smap) $ do
    pad <- if | even (length smap) -> return []
              | otherwise          -> (:[]) <$> genTmp TInt
    stackMap .= smap ++ pad ++ [x,x]

locate :: Id -> Caml [Int]
locate x = uses stackMap loc
  where loc [] = []
        loc (y:zs) | x == y    = 0 : map succ (loc zs)
                   | otherwise = map succ (loc zs)
offset :: Id -> Caml Int
offset x = (4*) . head <$> locate x

stackSize :: Caml Int
stackSize = uses stackMap (align . (4*) . length)

ppIdOrImm :: IdOrImm -> String
ppIdOrImm (V x) = x
ppIdOrImm (C i) = "$" ++ show i

shuffle :: Id -> [(Id,Id)] -> [(Id,Id)]
shuffle sw xys =
  let (_,xys') = partition (\(x,y) -> x==y) xys
                --等しくない奴
      tmp  = partition (\(_,y) -> memAssoc y xys) xys'
                --等しくない奴でyが定義域にないやつ
  in case tmp of
    ([],[]) -> []
    ((x,y):xys,[]) ->
        let f (y',z) | y == y'   = (sw,z)
                     | otherwise = (y',z)
        in (y,sw) : (x,y) : shuffle sw (map f xys)
    (xys,acyc) -> acyc ++ shuffle sw xys

data Dest = Tail | NonTail Id

g :: Handle -> (Dest,Asm) -> Caml ()
g h = \case
  (dest, AsmAns exp) -> g' h (dest,exp)
  (dest, AsmLet (x,t) exp e) -> g' h (NonTail x, exp) >> g h (dest, e)

g' :: Handle -> (Dest,AExpr) -> Caml ()
g' h (dest,exp) =
  let write s = liftIO $ hPutStr h s
  in case dest of
    -- Nontailなら結果をdestにセットする.
    NonTail x -> case exp of
      ANop -> return ()
      ASet i ->
          write $ printf "\tmovl\t$%d, %s\n" i x
      ASetL (Label y) ->
          write $ printf "\tmovl\t$%s, %s\n" y x
      AMov y
        | x /= y -> write $ printf "\tmovl\t%s, %s\n" y x
        | otherwise -> return ()
      ANeg y -> do
          when (x /= y) $
              write $ printf "\tmovl\t%s, %s\n" y x;
          write $ printf "\tnegl\t%s\n" x
      AAdd y z'
        | V(x) == z' -> do
            write $ printf "\taddl\t%s, %s\n" y x
        | otherwise -> do
            when (x /= y) $ write $ printf "\tmovl\t%s, %s\n" y x;
            write $ printf "\taddl\t%s, %s\n" (ppIdOrImm z') x
      ASub y z'
        | V(x) == z' -> do
            write $ printf "\tsubl\t%s, %s\n" y x
            write $ printf "\tnegl\t%s\n" x
        | otherwise -> do
            when (x /= y) $ write $ printf "\tmovl\t%s, %s\n" y x
            write $ printf "\tsubl\t%s, %s\n" (ppIdOrImm z') x
      ALd y (V z) i -> do
          write $ printf "\tmovl\t(%s,%s,%d), %s\n" y z i x
      ALd y (C j) i -> do
          write $ printf "\tmovl\t%d(%s), %s\n" (j * i) y x

      ASt x y (V z) i -> do
          write $ printf "\tmovl\t%s, (%s,%s,%d)\n" x y z i
      ASt x y (C j) i -> do
          write $ printf "\tmovl\t%s, %d(%s)\n" x (j * i) y

      AFMovD y ->
          when (x /= y) $ write $ printf "\tmovsd\t%s, %s\n" y x
      AFNegD y -> do
          when (x /= y) $ write $ printf "\tmovsd\t%s, %s\n" y x
          write $ printf "\txorpd\tmin_caml_fnegd, %s\n" x

      AFAddD y z
        | x == z -> do
            write $ printf "\taddsd\t%s, %s\n" y x
        | otherwise -> do
            when (x /= y) $ write $ printf "\tmovsd\t%s, %s\n" y x
            write $ printf "\taddsd\t%s, %s\n" z x
      AFSubD y z 
        | x == z -> do
            ss <- stackSize
            write $ printf "\tmovsd\t%s, %d(%s)\n" z ss regSp
            when (x /= y) $ write $ printf "\tmovsd\t%s, %s\n" y x
            write $ printf "\tsubsd\t%d(%s), %s\n" ss regSp x
        | otherwise -> do
            when (x /= y) $ write $ printf "\tmovsd\t%s, %s\n" y x
            write $ printf "\tsubsd\t%s, %s\n" z x
      AFMulD y z
        | x == z -> do
            write $ printf "\tmulsd\t%s, %s\n" y x
        | otherwise -> do
            when (x /= y) $ write $ printf "\tmovsd\t%s, %s\n" y x
            write $ printf "\tmulsd\t%s, %s\n" z x
      AFDivD y z
        | x == z -> do
            ss <- stackSize
            write $ printf "\tmovsd\t%s, %d(%s)\n" z ss regSp
            when (x /= y) $ write $ printf "\tmovsd\t%s, %s\n" y x
            write $ printf "\tdivsd\t%d(%s), %s\n" ss regSp x
        | otherwise -> do
            when (x /= y) $ write $ printf "\tmovsd\t%s, %s\n" y x
            write $ printf "\tdivsd\t%s, %s\n" z x
      ALdDF y (V z) i -> do
          write $ printf "\tmovsd\t(%s,%s,%d), %s\n" y z i x
      ALdDF y (C j) i -> do
          write $ printf "\tmovsd\t%d(%s), %s\n" (j * i) y x
      AStDF x y (V z) i -> do
          write $ printf "\tmovsd\t%s, (%s,%s,%d)\n" x y z i
      AStDF x y (C j) i -> do
          write $ printf "\tmovsd\t%s, %d(%s)\n" x (j * i) y
      AComment s -> do
          write $ printf "\t# %s\n" s
      ASave x y
        | x `elem` allRegs -> do
            sset <- use stackSet
            when (S.notMember y sset) $ do
              save y
              offset_y <- offset y
              write $ printf "\tmovl\t%s, %d(%s)\n" x offset_y regSp
        | x `elem` allFRegs -> do
            sset <- use stackSet
            when (S.notMember y sset) $ do
              savef y
              offset_y <- offset y
              write $ printf "\tmovsd\t%s, %d(%s)\n" x offset_y regSp
        | otherwise -> do
            sset <- use stackSet
            assert (S.member y sset) (return ())

      ARestore y
        | x `elem` allRegs -> do
            offset_y <- offset y
            write $ printf "\tmovl\t%d(%s), %s\n" offset_y regSp x
        | x `elem` allFRegs -> do
            offset_y <- offset y
            write $ printf "\tmovsd\t%d(%s), %s\n" offset_y regSp x
        | otherwise -> assert False undefined

      AIfEq y z' e1 e2 -> do
          write $ printf "\tcmpl\t%s, %s\n" (ppIdOrImm z') y;
          g'_non_tail_if h (NonTail x) e1 e2 "je" "jne"
      AIfLe y z' e1 e2 -> do
          write $ printf "\tcmpl\t%s, %s\n" (ppIdOrImm z') y;
          g'_non_tail_if h (NonTail x) e1 e2 "jle" "jg"
      AIfGe y z' e1 e2 -> do
          write $ printf "\tcmpl\t%s, %s\n" (ppIdOrImm z') y;
          g'_non_tail_if h (NonTail x) e1 e2 "jge" "jl"

      AIfFEq y z e1 e2 -> do
          write $ printf "\tcomisd\t%s, %s\n" z y;
          g'_non_tail_if h (NonTail x) e1 e2 "je" "jne"
      AIfFLe y z e1 e2 -> do
          write $ printf "\tcomisd\t%s, %s\n" z y;
          g'_non_tail_if h (NonTail x) e1 e2 "jbe" "ja"

      ACallCls y zs ws -> do
          g'_args h [(y, regCl)] zs ws
          ss <- stackSize
          when (ss>0) $ write $ printf "\taddl\t$%d, %s\n" ss regSp
          write $ printf "\tcall\t*(%s)\n" regCl
          when (ss>0) $ write $ printf "\tsubl\t$%d, %s\n" ss regSp
          if | (x `elem` allRegs && x /= regs!0) ->
                    write $ printf "\tmovl\t%s, %s\n" (regs!0) x
             | (x `elem` allFRegs && x /= fregs!0) ->
                    write $ printf "\tmovsd\t%s, %s\n" (fregs!0) x
             | otherwise -> return ()

      ACallDir (Label y) zs ws -> do
          g'_args h [] zs ws;
          ss <- stackSize
          when (ss>0) $ write $ printf "\taddl\t$%d, %s\n" ss regSp
          write $ printf "\tcall\t%s\n" y
          when (ss>0) $ write $ printf "\tsubl\t$%d, %s\n" ss regSp
          if | x `elem` allRegs && x /= regs!0 ->
                    write $ printf "\tmovl\t%s, %s\n" (regs!0) x
             | x `elem` allFRegs && x /= fregs!0 ->
                    write $ printf "\tmovsd\t%s, %s\n" (fregs!0) x
             | otherwise -> return ()

    -- 末尾だったら計算結果を第一レジスタにセットしてret
    Tail -> case exp of
      ANop -> do
          tmp <- genTmp TUnit
          g' h ((NonTail tmp), exp)
          write $ printf "\tret\n"
      ASt{} -> do
          tmp <- genTmp TUnit
          g' h ((NonTail tmp), exp)
          write $ printf "\tret\n"
      AStDF{} -> do
          tmp <- genTmp TUnit
          g' h ((NonTail tmp), exp)
          write $ printf "\tret\n"
      AComment{} -> do
          tmp <- genTmp TUnit
          g' h ((NonTail tmp), exp)
          write $ printf "\tret\n"
      ASave {} -> do
          tmp <- genTmp TUnit
          g' h ((NonTail tmp), exp)
          write $ printf "\tret\n"

      ASet{} -> do
          g' h (NonTail (regs!0), exp)
          write $ printf "\tret\n";
      ASetL{} -> do
          g' h (NonTail (regs!0), exp)
          write $ printf "\tret\n";
      AMov{} -> do
          g' h (NonTail (regs!0), exp)
          write $ printf "\tret\n";
      ANeg{} -> do
          g' h (NonTail (regs!0), exp)
          write $ printf "\tret\n";
      AAdd{} -> do
          g' h (NonTail (regs!0), exp)
          write $ printf "\tret\n";
      ASub{} -> do
          g' h (NonTail (regs!0), exp)
          write $ printf "\tret\n";
      ALd{} -> do
          g' h (NonTail (regs!0), exp)
          write $ printf "\tret\n";

      AFMovD{} -> do
          g' h (NonTail(fregs!0), exp);
          write $ printf "\tret\n";
      AFNegD{} -> do
          g' h (NonTail(fregs!0), exp);
          write $ printf "\tret\n";
      AFAddD{} -> do
          g' h (NonTail(fregs!0), exp);
          write $ printf "\tret\n";
      AFSubD{} -> do
          g' h (NonTail(fregs!0), exp);
          write $ printf "\tret\n";
      AFMulD{} -> do
          g' h (NonTail(fregs!0), exp);
          write $ printf "\tret\n";
      AFDivD{} -> do
          g' h (NonTail(fregs!0), exp);
          write $ printf "\tret\n";
      ALdDF{} -> do
          g' h (NonTail(fregs!0), exp);
          write $ printf "\tret\n";

      ARestore x -> do
          lx <- locate x
          case lx of
            [i]   -> g' h (NonTail(regs!0), exp)
            [i,j] -> assert (i+1==j) $ g' h (NonTail(fregs!0), exp)
            _ -> assert False undefined
          write $ printf "\tret\n"

      AIfEq x y' e1 e2 -> do
          write $ printf "\tcmpl\t%s, %s\n" (ppIdOrImm y') x
          g'_tail_if h e1 e2 "je" "jne"
      AIfLe x y' e1 e2 -> do
          write $ printf "\tcmpl\t%s, %s\n" (ppIdOrImm y') x
          g'_tail_if h e1 e2 "jle" "jg"
      AIfGe x y' e1 e2 -> do
          write $ printf "\tcmpl\t%s, %s\n" (ppIdOrImm y') x
          g'_tail_if h e1 e2 "jge" "jl"
      AIfFEq x y e1 e2 -> do
          write $ printf "\tcomisd\t%s, %s\n" y x
          g'_tail_if h e1 e2 "je" "jne"
      AIfFLe x y e1 e2 -> do
          write $ printf "\tcomisd\t%s, %s\n" y x
          g'_tail_if h e1 e2 "jbe" "ja"

      ACallCls x ys zs -> do
          g'_args h [(x, regCl)] ys zs
          write $ printf "\tjmp\t*(%s)\n" regCl;
      ACallDir (Label x) ys zs -> do
          g'_args h [] ys zs
          write $ printf "\tjmp\t%s\n" x

g'_tail_if :: Handle -> Asm -> Asm -> Id -> Id -> Caml ()
g'_tail_if h e1 e2 b bn = do
  let write s = liftIO $ hPutStr h s
  b_else <- genId (b ++ "_else")
  write $ printf "\t%s\t%s\n" bn b_else
  ssetBak <- use stackSet
  g h (Tail, e1)
  write $ printf "%s:\n" b_else
  stackSet .= ssetBak
  g h (Tail, e2)

g'_non_tail_if :: Handle -> Dest -> Asm -> Asm -> Id -> Id -> Caml ()
g'_non_tail_if h dest e1 e2 b bn = do
  let write s = liftIO $ hPutStr h s
  b_else <- genId (b ++ "_else")
  b_cont <- genId (b ++ "_cont")
  write $ printf "\t%s\t%s\n" bn b_else
  ssetBak <- use stackSet
  g h (dest, e1)
  sset1 <- use stackSet
  write $ printf "\tjmp\t%s\n" b_cont;
  write $ printf "%s:\n" b_else;
  stackSet .= ssetBak
  g h (dest, e2)
  write $ printf "%s:\n" b_cont;
  sset2 <- use stackSet
  stackSet .= S.intersection sset1 sset2

g'_args :: Handle -> [(Id,Id)] -> [Id] -> [Id] -> Caml ()
g'_args h xRegCl ys zs =
  assert (length ys <= length regs - length xRegCl) $
  assert (length zs <= length fregs) $ do
  ss <- stackSize
  let write s = liftIO $ hPutStr h s
      sw = printf "%d(%s)" ss regSp :: String
      (i,yrs) = foldl' f (0,xRegCl) ys
          where f (i,yrs) y = (i+1, (y,regs!i) : yrs)
      (d,zfrs) = foldl' f (0,[]) zs
          where f (d,zfrs) z = (d+1, (z,fregs!d) : zfrs)
  forM_ (shuffle sw yrs)  $ \(y,r)  -> write $ printf "\tmovl\t%s, %s\n" y r
  forM_ (shuffle sw zfrs) $ \(z,fr) -> write $ printf "\tmovsd\t%s, %s\n" z fr

h :: Handle -> AFunDef -> Caml ()
h handle (AFunDef (Label x) _ _ e _) = do
  let write s = liftIO $ hPutStr handle s
  write $ printf "%s:\n" x
  stackSet .= S.empty
  stackMap .= []
  g handle (Tail, e)

emit :: Handle -> AProg -> Caml ()
emit handle (AProg fdata fundefs e) = do
  let write s = liftIO $ hPutStr handle s
  liftIO $ putStrLn "generating assembly..."

  write $ printf ".data\n"
  write $ printf ".balign\t8\n"
  forM_ fdata $ \(Label x,d) -> do
      write $ printf "%s:\t# %.6f\n" x d
      write $ printf "\t.long\t0x%lx\n" (gethi d)
      write $ printf "\t.long\t0x%lx\n" (getlo d)
  write $ printf ".text\n"
  forM_ fundefs $ \fundef -> h handle fundef
  write $ printf ".globl\tmin_caml_start\n"
  write $ printf "min_caml_start:\n"
  write $ printf ".globl\t_min_caml_start\n"
  write $ printf "_min_caml_start: # for cygwin\n"
  write $ printf "\tpushl\t%%eax\n"
  write $ printf "\tpushl\t%%ebx\n"
  write $ printf "\tpushl\t%%ecx\n"
  write $ printf "\tpushl\t%%edx\n"
  write $ printf "\tpushl\t%%esi\n"
  write $ printf "\tpushl\t%%edi\n"
  write $ printf "\tpushl\t%%ebp\n"
  write $ printf "\tmovl\t32(%%esp),%s\n" regSp
  write $ printf "\tmovl\t36(%%esp),%s\n" (regs!0)
  write $ printf "\tmovl\t%s,%s\n" (regs!0) regHp
  stackSet .= S.empty
  stackMap .= []
  g handle (NonTail(regs!0), e)
  write $ printf "\tpopl\t%%ebp\n"
  write $ printf "\tpopl\t%%edi\n"
  write $ printf "\tpopl\t%%esi\n"
  write $ printf "\tpopl\t%%edx\n"
  write $ printf "\tpopl\t%%ecx\n"
  write $ printf "\tpopl\t%%ebx\n"
  write $ printf "\tpopl\t%%eax\n"
  write $ printf "\tret\n"


----------
-- Util --
----------

memAssoc :: Eq a => a -> [(a,b)] -> Bool
memAssoc x [] = False
memAssoc x ((y,_):zs)
  | x==y      = True
  | otherwise = memAssoc x zs


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module BackEnd.X86.Asm where

import Base

import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Control.Lens

-----------------
-- Asm.t = Asm --
-----------------
type Immediate = Int
data IdOrImm = V Id
             | C Immediate
             deriving (Show, Eq)

 -- 命令の列
data Asm = AsmAns AExpr
         | AsmLet (Id, Type) AExpr Asm
         deriving Show

-- x86の各命令に対応
data AExpr = ANop
           | ASet Int
           | ASetL Label
           | AMov Id
           | ANeg Id
           | AAdd Id IdOrImm
           | ASub Id IdOrImm
           | ALd Id IdOrImm Int
           | ASt Id Id IdOrImm Int
           | AFMovD Id
           | AFNegD Id
           | AFAddD Id Id
           | AFSubD Id Id
           | AFMulD Id Id
           | AFDivD Id Id
           | ALdDF Id IdOrImm Int
           | AStDF Id Id IdOrImm Int
           | AComment String
           | AIfEq Id IdOrImm Asm Asm
           | AIfLe Id IdOrImm Asm Asm
           | AIfGe Id IdOrImm Asm Asm
           | AIfFEq Id Id Asm Asm
           | AIfFLe Id Id Asm Asm
           | ACallCls Id    [Id] [Id]
           | ACallDir Label [Id] [Id]
           | ASave Id Id
           | ARestore Id
           deriving Show

data AFunDef = AFunDef { _aname  :: Label
                       , _aargs  :: [Id]
                       , _afargs :: [Id]
                       , _abody  :: Asm
                       , _aret   :: Type }
             deriving Show
makeLenses ''AFunDef

data AProg = AProg [(Label, Double)] [AFunDef] Asm
           deriving Show



fLetD :: (Id, AExpr, Asm) -> Asm
fLetD (x,e1,a2) = AsmLet (x,TFloat) e1 a2

seq' :: (AExpr, Asm) -> Caml Asm
seq' (e1,a2) = do
  x <- genTmp TUnit
  return $ AsmLet (x,TUnit) e1 a2

regs :: Vector String
regs = V.fromList ["%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi"]

fregs :: Vector String
fregs = V.generate 8 $ \i -> "%xmm" ++ show i

allRegs :: [String]
allRegs = V.toList regs

allFRegs :: [String]
allFRegs = V.toList fregs

-- closure address
regCl :: String
regCl = V.last regs
--regCl = regs ! (V.length regs -1)

-- stack pointer
regSp :: String
regSp = "%ebp"

-- heap pointer
regHp :: String
regHp = "min_caml_hp"

---- return address
--regRa :: String
--regRa = "%eax"

isReg :: String -> Bool
isReg ('%':_) = True
isReg x
  | x == "min_caml_hp" = True
  | otherwise          = False

-- super-tenuki? らしい
removeAndUniq :: Set String -> [String] -> [String]
removeAndUniq xs = \case
  [] -> []
  y:ys | S.member y xs -> removeAndUniq xs ys
       | otherwise     -> y : removeAndUniq (S.insert y xs) ys

fv :: Asm -> [String]
fv e = removeAndUniq S.empty (fv' e)

fv' :: Asm -> [String]
fv' (AsmAns a) = fvAExpr a
fv' (AsmLet (x,_t) e a) = fvAExpr e ++ removeAndUniq (S.singleton x) (fv a)

fvOfIdOrImm :: IdOrImm -> [String]
fvOfIdOrImm (V x) = [x]
fvOfIdOrImm (C _) = []

fvAExpr :: AExpr -> [String]
fvAExpr = \case
  ANop       -> []
  ASet{}     -> []
  ASetL{}    -> []
  AComment{} -> []
  ARestore{} -> []

  AMov x    -> [x]
  ANeg x    -> [x]
  AFMovD x  -> [x]
  AFNegD x  -> [x]
  ASave x _ -> [x]

  AAdd x y'    -> x : fvOfIdOrImm y'
  ASub x y'    -> x : fvOfIdOrImm y'
  ALd x y' _   -> x : fvOfIdOrImm y'
  ALdDF x y' _ -> x : fvOfIdOrImm y'

  ASt x y z' _   -> x : y : fvOfIdOrImm z'
  AStDF x y z' _ -> x : y : fvOfIdOrImm z'

  AFAddD x y -> [x,y]
  AFSubD x y -> [x,y]
  AFMulD x y -> [x,y]
  AFDivD x y -> [x,y]

  AIfEq x y' e1 e2 -> x : fvOfIdOrImm y' ++ removeAndUniq S.empty (fv' e1 ++ fv' e2)
  AIfLe x y' e1 e2 -> x : fvOfIdOrImm y' ++ removeAndUniq S.empty (fv' e1 ++ fv' e2)
  AIfGe x y' e1 e2 -> x : fvOfIdOrImm y' ++ removeAndUniq S.empty (fv' e1 ++ fv' e2)

  AIfFEq x y e1 e2 -> x : y : removeAndUniq S.empty (fv' e1 ++ fv' e2)
  AIfFLe x y e1 e2 -> x : y : removeAndUniq S.empty (fv' e1 ++ fv' e2)

  ACallCls x ys zs -> x : ys ++ zs
  ACallDir _ ys zs -> ys ++ zs

concat' :: Asm -> (Id,Type) -> Asm -> Asm
concat' a1 xt a2 = case a1 of
  AsmAns e -> AsmLet xt e a2
  AsmLet yt e a1' -> AsmLet yt e (concat' a1' xt a2)

align :: Int -> Int
align i | i `mod` 8 == 0 = i
        | otherwise      = i+4


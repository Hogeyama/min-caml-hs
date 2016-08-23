{-# LANGUAGE TemplateHaskell #-}

module AllTypes where

import Data.IORef
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class as IOC
import Control.Monad.Except (throwError, catchError)
import System.IO.Unsafe (unsafePerformIO)

---------------
-- Id.t = Id --
---------------
type Id = String
newtype Label = Label String
              deriving Show


-------------------
-- Type.t = Type --
-------------------
data Type = TUnit
          | TBool
          | TInt
          | TFloat
          | TFun   [Type] Type
          | TTuple [Type]
          | TArray Type
          | TVar   TypeRef
          deriving (Show, Eq)
type TyEnv = Map Id Type

type TypeRef = IORef (Maybe Type)

instance Show a => Show (IORef a) where
  show ref = "Ref (" ++ unsafePerformIO (show <$> readIORef ref) ++ ")"


---------------------
-- Syntax.t = Expr --
---------------------
data Expr = EUnit
          | EBool     Bool
          | EInt      Int
          | EFloat    Double
          | ENot      Expr
          | ENeg      Expr
          | EAdd      Expr Expr
          | ESub      Expr Expr
          | EFNeg     Expr
          | EFAdd     Expr Expr
          | EFSub     Expr Expr
          | EFMul     Expr Expr
          | EFDiv     Expr Expr
          | EEq       Expr Expr
          | ELe       Expr Expr
          | EIf       Expr Expr Expr
          | ELet      (Id, Type) Expr Expr
          | EVar      Id
          | ELetRec   FunDef Expr
          | EApp      Expr [Expr]
          | ETuple    [Expr]
          | ELetTuple [(Id,Type)] Expr Expr
          | EArray    Expr Expr
          | EGet      Expr Expr
          | EPut      Expr Expr Expr
          deriving (Show, Eq)

data FunDef = FunDef { _name :: (Id, Type)
                     , _args :: [(Id,Type)]
                     , _body :: Expr
                     }
            deriving (Show, Eq)
makeLenses ''FunDef

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
makeLenses ''KFunDef

-----------------------
-- Closure.t = CExpr --
-----------------------
data CExpr = CUnit
           | CInt      Int
           | CFloat    Double
           | CNeg      Id
           | CAdd      Id Id
           | CSub      Id Id
           | CFNeg     Id
           | CFAdd     Id Id
           | CFSub     Id Id
           | CFMul     Id Id
           | CFDiv     Id Id
           | CIfEq     Id Id     CExpr CExpr
           | CIfLe     Id Id     CExpr CExpr
           | CLet      (Id,Type) CExpr CExpr
           | CVar      Id
           | CMakeCls  (Id,Type) Closure CExpr
           | CAppCls   Id [Id]
           | CAppDir   Label [Id]
           | CTuple    [Id]
           | CLetTuple [(Id,Type)] Id CExpr
           | CGet      Id Id
           | CPut      Id Id Id
           | CExtArray Label
           deriving Show
data Closure = Closure { _entry    :: Label
                       , _actualFV :: [Id]}
             deriving Show
data CProg = CProg [CFunDef] CExpr
           deriving Show
data CFunDef = CFunDef { _cname     :: (Label,Type)
                       , _cargs     :: [(Id,Type)]
                       , _cFormalFV :: [(Id,Type)]
                       , _cbody     :: CExpr}
              deriving Show
makeLenses ''CFunDef


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


----------------
-- Main Monad --
----------------
type Caml a = StateT S (ExceptT Error IO) a
data Error = Failure String
           | Unify Type Type
           | Typing Expr Type Type
           | NoReg Id Type
           deriving Show

-- closure
data S = S { _idCount :: Int
           , _tvCount :: Int
           , _extTyEnv  :: TyEnv
           , _threshold :: Int                -- inline size
           , _closureToplevel :: [CFunDef]
           , _virtualData :: [(Label,Double)]
           , _stackSet :: Set Id -- for Emit module
           , _stackMap :: [Id]   -- for Emit module
           , _optimiseLimit :: Int
           }
           deriving Show
makeLenses ''S

initialState :: S
initialState = S { _idCount = 0
                 , _tvCount = 0
                 , _extTyEnv = M.empty
                 , _threshold = 0
                 , _closureToplevel = []
                 , _virtualData = []
                 , _stackSet = S.empty
                 , _stackMap = []
                 , _optimiseLimit = 100
                 }

liftIO :: IO a -> Caml a
liftIO = IOC.liftIO

throw :: Error -> Caml a
throw = throwError

-- NOTE: m中の状態変化はなかったことになる 気をつけて使う
-- catch m h = StateT $ \s -> runStateT m s `catchE` \e -> runStateT (h e) s
catch :: Caml a -> (Error -> Caml a) -> Caml a
catch = catchError

runCamlDefault :: Caml a -> IO (Either Error a)
runCamlDefault c = runExceptT $ evalStateT c initialState

runCaml :: Caml a -> S -> IO (Either Error a)
runCaml c s = runExceptT $ evalStateT c s

liftEither :: Either Error a -> Caml a
liftEither (Left e)  = throw e
liftEither (Right a) = return a


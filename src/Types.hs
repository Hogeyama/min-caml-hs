{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.IORef
import Control.Lens
import Data.Map as M
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
          | TVar   (IORef (Maybe Type))
          deriving (Show, Eq)
type TyEnv = M.Map Id Type

instance Show a => Show (IORef a) where
  show ref = "Ref (" ++ unsafePerformIO (show <$> readIORef ref) ++ ")"
type TypeRef = IORef (Maybe Type)


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



----------------
-- Main Monad --
----------------
type Caml a = StateT S (ExceptT Error IO) a
data Error = Failure String
           | Unify Type Type
           | Typing Expr Type Type
           deriving Show

-- closure
data S = S { _idCount :: Int
           , _extTyEnv  :: TyEnv
           , _threshold :: Int   -- inline size
           , _closureToplevel :: [CFunDef]
           }
makeLenses ''S

initialState :: S
initialState = S 0 M.empty 0 []

liftIO :: IO a -> Caml a
liftIO = IOC.liftIO

throw :: Error -> Caml a
throw = throwError

-- NOTE: m中の状態変化はなかったことになる 気をつけて使う
-- catch m h = StateT $ \s -> runStateT m s `catchE` \e -> runStateT (h e) s
catch :: Caml a -> (Error -> Caml a) -> Caml a
catch = catchError

runCaml :: Caml a -> IO (Either Error a)
runCaml c = runExceptT $ evalStateT c initialState

runCamlWith :: Caml a -> S -> IO (Either Error a)
runCamlWith c s = runExceptT $ evalStateT c s

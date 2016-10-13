{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Base where

import                Data.IORef
import                Control.Lens
import                Data.Map                       (Map)
import qualified      Data.Map as M
import                Data.Set                       (Set)
import qualified      Data.Set as S
import                Control.Monad.Trans.Except
import                Control.Monad.Trans.State.Lazy
import qualified      Control.Monad.IO.Class as IOC
import                Control.Monad.Except           (throwError, catchError)
import {-# SOURCE #-} FrontEnd.Syntax                (Expr)
import {-# SOURCE #-} MiddleEnd.Closure              (CFunDef)

-----------
-- Types --
-----------
type Id = String
newtype Label = Label String
              deriving Show

data Type = TUnit
          | TBool
          | TInt
          | TFloat
          | TFun   [Type] Type
          | TTuple [Type]
          | TArray Type
          | TVar   TV
          deriving (Show, Eq)
type TyEnv = Map Id Type

data TV = TV Int (IORef (Maybe Type))
          deriving (Eq)

instance Show TV where
  show (TV n _) = "tv" ++ show n

data S = S { _idCount :: Int                  -- for Id module
           , _tvCount :: Int                  -- for Typing module
           , _extTyEnv  :: TyEnv              -- for Typing module
           , _threshold :: Int                -- for Inline module (max inline size)
           , _closureToplevel :: [CFunDef]    -- for Closure module 
           , _virtualData :: [(Label,Double)] -- for Virtual module
           , _stackSet :: Set Id              -- for Emit module
           , _stackMap :: [Id]                -- for Emit module
           , _optimiseLimit :: Int            -- for Optimise module (max optimise iter)
           }
           deriving Show
makeLenses ''S

type Caml a = StateT S (ExceptT Error IO) a
data Error = Failure String
           | Unify Type Type
           | Typing Expr Type Type
           | NoReg Id Type
           deriving Show

---------------
-- functions --
---------------

-- Type --
genType :: Caml Type
genType = do
  ref <- liftIO $ newIORef Nothing
  n   <- tvCount <+= 1
  return $ TVar $ TV n ref

readType :: TV -> Caml (Maybe Type)
readType (TV _ ref) = liftIO $ readIORef ref

writeType :: TV -> Type -> Caml ()
writeType (TV _ ref) t = liftIO $ writeIORef ref (Just t)

-- Id --
ppList :: [Id] -> Id
ppList = \case
  []  -> ""
  [x] -> x
  x:xs -> x ++ " " ++ ppList xs

genId :: String -> Caml Id
genId s = do
  n <- idCount <+= 1
  return $ s ++ "." ++ show n

genTmp :: Type -> Caml Id
genTmp ty = do
  n <- idCount <+= 1
  return $ "T" ++ idOfType ty ++ show n

idOfType :: Type -> String
idOfType = \case
  TUnit    -> "u"
  TBool    -> "b"
  TInt     -> "i"
  TFloat   -> "d"
  TFun _ _ -> "f"
  TTuple _ -> "t"
  TArray _ -> "a"
  TVar _   -> error "idOfType: TVar"

-- Caml
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


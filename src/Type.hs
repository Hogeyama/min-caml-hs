
module Type where

import AllTypes
import Data.IORef
import Control.Lens

genType :: Caml Type
genType = do
  ref <- liftIO $ newIORef Nothing
  return $ TVar ref

readType :: TypeRef -> Caml (Maybe Type)
readType ref = liftIO $ readIORef ref

writeType :: TypeRef -> Type -> Caml ()
writeType ref t = liftIO $ writeIORef ref (Just t)


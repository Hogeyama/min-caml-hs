
module Type where

import AllTypes
import Data.IORef
import Control.Lens

genType :: Caml Type
genType = do
  ref <- liftIO $ newIORef Nothing
  n   <- tvCount <+= 1
  return $ TVar $ TV n ref

readType :: TV -> Caml (Maybe Type)
readType (TV _ ref) = liftIO $ readIORef ref

writeType :: TV -> Type -> Caml ()
writeType (TV _ ref) t = liftIO $ writeIORef ref (Just t)


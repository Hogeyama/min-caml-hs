
module Base where

import Data.IORef
type Id = String
data Type = TUnit
          | TBool
          | TInt
          | TFloat
          | TFun   [Type] Type
          | TTuple [Type]
          | TArray Type
          | TVar   TV
data TV = TV Int (IORef (Maybe Type))
newtype Label = Label String

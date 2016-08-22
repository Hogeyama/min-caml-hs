
module Main where
import Data.Word

foreign import ccall "gethi" gethi :: Float -> Word32
foreign import ccall "getlo" getlo :: Word32 -> Float

hoge :: Word32
hoge = gethi 1.0

main :: IO ()
main = do
  print hoge

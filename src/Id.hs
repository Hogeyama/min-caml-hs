{-# LANGUAGE LambdaCase #-}

module Id where

import CamlMonad
import Control.Lens

ppList :: [String] -> String
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

{-
main :: IO ()
main = do
  m <- runCaml $ do
      n  <- use idCount
      s1 <- genId "hoge"
      s2 <- genId "fuga"
      --idCount %= (const 100)
      idCount .= 100
      s3 <- genTmp TUnit
      liftIO $ putStrLn s1
      liftIO $ putStrLn s2
      --throw $ Failure ""
      liftIO $ putStrLn s3
    `catch` \e -> do
      s <- genId "piyo"
      liftIO $ putStrLn s
  case m of
    Left e   -> print e
    Right () -> putStrLn "ok"

|| hoge.1
|| fuga.2
|| piyo.1
|| ok
-}




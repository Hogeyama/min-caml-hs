{
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Lexer where
import AllTypes
import Control.Lens
import Control.Monad.State.Class
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]
@ident = $alpha ($alpha | $digit | \' | \_)*
@int   = $digit+
@float = $digit+ (\. $digit*)? ([e E] [\+ \-]? $digit+)?

tokens :-
  <0> $white+       { skip                  }
  <0> "(*"          { startComment          }
  <0> "("           { tk $ TokenLParen      }
  <0> ")"           { tk $ TokenRParen      }
  <0> true          { tk $ TokenBool True   }
  <0> false         { tk $ TokenBool False  }
  <0> not           { tk $ TokenNot         }
  <0> @int          { mktk TokenInt read    }
  <0> @float        { mktk TokenFloat readF }
  <0> "-"           { tk $ TokenMinus       }
  <0> "+"           { tk $ TokenPlus        }
  <0> "-."          { tk $ TokenMinusDot    }
  <0> "+."          { tk $ TokenPlusDot     }
  <0> "*."          { tk $ TokenAstDot      }
  <0> "/."          { tk $ TokenSlashDot    }
  <0> "="           { tk $ TokenEq          }
  <0> "<>"          { tk $ TokenLtGt        }
  <0> "<="          { tk $ TokenLe          }
  <0> "<="          { tk $ TokenGe          }
  <0> "<"           { tk $ TokenLt          }
  <0> ">"           { tk $ TokenGt          }
  <0> if            { tk $ TokenIf          }
  <0> then          { tk $ TokenThen        }
  <0> else          { tk $ TokenElse        }
  <0> let           { tk $ TokenLet         }
  <0> in            { tk $ TokenIn          }
  <0> rec           { tk $ TokenRec         }
  <0> ","           { tk $ TokenComma       }
  <0> "_"           { tk $ TokenWild        }
  <0> Array\.create { tk $ TokenArrayCreate }
  <0> "."           { tk $ TokenDot         }
  <0> "<-"          { tk $ TokenRArrow      }
  <0> ";"           { tk $ TokenSemi        }
  <0> @ident        { mktk TokenID id       }

  <comment> "(*"    { incCommentDepth       }
  <comment> "*)"    { decCommentDepth       }
  <comment> .       { skip                  }
  <comment> \n      { skip                  } --別枠らしい

{
data Token = TokenBool Bool
           | TokenInt Int
           | TokenFloat Double
           | TokenNot
           | TokenMinus
           | TokenPlus
           | TokenMinusDot
           | TokenPlusDot
           | TokenAstDot
           | TokenSlashDot
           | TokenEq
           | TokenLtGt
           | TokenLe
           | TokenGe
           | TokenLt
           | TokenGt
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenID String
           | TokenLet
           | TokenIn
           | TokenRec
           | TokenComma
           | TokenArrayCreate
           | TokenDot
           | TokenRArrow
           | TokenSemi
           | TokenLParen
           | TokenRParen
           | TokenEOF
           | TokenWild
           deriving (Eq,Show)

lex :: String -> Caml [Token]
lex s = case runAlex s scanTokens of
  Left e -> throw $ Failure e
  Right tks -> return tks

scanTokens :: Alex [Token]
scanTokens = loop []
  where loop acc = alexMonadScan >>= \case
            TokenEOF -> return $ reverse acc
            t -> loop $ t:acc

alexEOF :: Alex Token
alexEOF = do
    code <- alexGetStartCode
    if | code == 0       -> return TokenEOF
       | code == comment -> alexError "comment not terminated"

tk :: Token -> AlexAction Token
tk t = token $ \_ _ -> t

mktk :: (a -> Token) -> (String -> a) -> AlexAction Token
mktk con read = token $ \(_,_,_,s) n -> con $ read $ take n s

readF :: String -> Double
readF s
  | last s == '.' = read (s++['0'])
  | otherwise     = read s

-- commentまわり
newtype AlexUserState = AUS { depth :: Int }
commentDepth :: Lens' AlexUserState Int
commentDepth = lens depth $ \_ d -> AUS d
alexInitUserState :: AlexUserState
alexInitUserState = AUS 0

instance MonadState AlexUserState Alex where
    state f = Alex $ \s ->
      let ust = alex_ust s
          (a, ust') = f ust
          s' = s { alex_ust = ust' }
      in  Right (s', a)

startComment :: AlexAction Token
startComment = \_ _ -> do
    commentDepth .= 1
    alexSetStartCode comment
    alexMonadScan

incCommentDepth :: AlexAction Token
incCommentDepth = \_ _ -> do
    commentDepth += 1
    alexMonadScan

decCommentDepth :: AlexAction Token
decCommentDepth = \_ _ -> do
    d <- commentDepth <-= 1
    if d == 0 then do
        alexSetStartCode 0
        alexMonadScan
    else do
        alexMonadScan

}


{
module Lexer where
import AllTypes
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
@ident = $alpha ($alpha | $digit | \' | \_)*
@int   = $digit+
@float = $digit+ (\. $digit*)? ([e E] [\+ \-]? $digit+)?

tokens :-
  $white+ ;
  \(            { \s -> TokenLParen              }
  \)            { \s -> TokenRParen              }
  true          { \s -> TokenBool True           }
  false         { \s -> TokenBool False          }
  not           { \s -> TokenNot                 }
  @int          { \s -> TokenInt (read s)        }
  @float        { \s -> TokenFloat (readFloat s) }
  \-            { \s -> TokenMinus               }
  \+            { \s -> TokenPlus                }
  \-\.          { \s -> TokenMinusDot            }
  \+\.          { \s -> TokenPlusDot             }
  \*\.          { \s -> TokenAstDot              }
  \/\.          { \s -> TokenSlashDot            }
  \=            { \s -> TokenEq                  }
  \<\>          { \s -> TokenLtGt                }
  \<\=          { \s -> TokenLe                  }
  \<\=          { \s -> TokenGe                  }
  \<            { \s -> TokenLt                  }
  \>            { \s -> TokenGt                  }
  if            { \s -> TokenIf                  }
  then          { \s -> TokenThen                }
  else          { \s -> TokenElse                }
  let           { \s -> TokenLet                 }
  in            { \s -> TokenIn                  }
  rec           { \s -> TokenRec                 }
  \,            { \s -> TokenComma               }
  \_            { \s -> TokenWild                }
        -- TODO
        --  IDENT(Id.gentmp Type.Unit)
        -- に変換する, Parserでやるのが楽かな?
  Array\.create { \s -> TokenArrayCreate    }
  \.            { \s -> TokenDot            }
  \<\-          { \s -> TokenRArrow         }
  \;            { \s -> TokenSemi           }
  @ident        { \s -> TokenID s           }

{
-- The token type:
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

scanTokens :: String -> Either Error [Token]
scanTokens str = go ('\n',[],str)
  where
    go inp@(_,_bs,s) =
      case alexScan inp 0 of
        AlexEOF -> Right []
        AlexSkip  inp' len     -> go inp'
        AlexToken inp' len act -> do
            l' <- go inp'
            return $ act (take len s) : l'
        AlexError _ -> Left $ Failure "lexical error"

readFloat :: String -> Double
readFloat s
    | last s == '.' = read (s++['0'])
    | otherwise     = read s
}

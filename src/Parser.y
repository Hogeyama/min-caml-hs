{
module Parser where

import Lexer
import AllTypes
import Id
import Type
}

%name parse
%tokentype { Token }
%monad { Caml } { (>>=) } { return }
%error { parseError }

%token
     bool        { TokenBool $$     }
     int         { TokenInt $$      }
     float       { TokenFloat $$    }
     not         { TokenNot         }
     '-'         { TokenMinus       }
     '+'         { TokenPlus        }
     '-.'        { TokenMinusDot    }
     '+.'        { TokenPlusDot     }
     '*.'        { TokenAstDot      }
     '/.'        { TokenSlashDot    }
     '='         { TokenEq          }
     '<>'        { TokenLtGt        }
     '<='        { TokenLe          }
     '>='        { TokenGe          }
     '<'         { TokenLt          }
     '>'         { TokenGt          }
     if          { TokenIf          }
     then        { TokenThen        }
     else        { TokenElse        }
     id          { TokenID $$       }
     let         { TokenLet         }
     in          { TokenIn          }
     rec         { TokenRec         }
     ','         { TokenComma       }
     ArrayCreate { TokenArrayCreate }
     '.'         { TokenDot         }
     '<-'        { TokenRArrow      }
     ';'         { TokenSemi        }
     '('         { TokenLParen      }
     ')'         { TokenRParen      }
     eof         { TokenEOF         }
     wild        { TokenWild        }
%right prec_let
%right ';'
%right prec_if
%right '<-'
%left  ','
%left  '=' '<>' '<=' '<' '>' '>='
%left  '+' '-' '+.' '-.'
%left  '*.' '/.'
%left  prec_neg
%left  prec_app
%left  '.'

%nonassoc '>' '<'
%left '+' '-'
%left '*' '/'

%%

Expr :: { Expr }
    : SimpleExpr                                       { $1                 }
    | not Expr %prec prec_app                          { ENot $2            }
    | '-' Expr  %prec prec_neg                         { neg $2             }
    | Expr '+' Expr                                    { EAdd $1 $3         }
    | Expr '-' Expr                                    { ESub $1 $3         }
    | Expr '=' Expr                                    { EEq  $1 $3         }
    | Expr '<>' Expr                                   { ENot (EEq $1 $3)   }
    | Expr '<' Expr                                    { ENot (ELe $3 $1)   }
    | Expr '>' Expr                                    { ENot (ELe $1 $3)   }
    | Expr '<=' Expr                                   { ELe $1 $3          }
    | Expr '>=' Expr                                   { ELe $3 $1          }
    | if Expr then Expr else Expr %prec prec_if        { EIf $2 $4 $6       }
    | '-.' Expr %prec prec_neg                         { EFNeg $2           }
    | Expr '+.' Expr                                   { EFAdd $1 $3        }
    | Expr '-.' Expr                                   { EFSub $1 $3        }
    | Expr '*.' Expr                                   { EFMul $1 $3        }
    | Expr '/.' Expr                                   { EFDiv $1 $3        }
    | let ID '=' Expr in Expr %prec prec_let           { % elet $2 $4 $6    }
    | let rec FunDef in Expr %prec prec_let            { ELetRec $3 $5      }
    | Expr ActualArgs %prec prec_app                   { EApp $1 $2         }
    | Elems                                            { ETuple $1          }
    | let '(' Pat ')' '=' Expr in Expr                 { ELetTuple $3 $6 $8 }
    | SimpleExpr '.' '(' Expr ')' '<-' Expr            { EPut $1 $4 $7      }
    | Expr ';' Expr                                    { % eseq $1 $3       }
    | ArrayCreate SimpleExpr SimpleExpr %prec prec_app { EArray $2 $3       }
    | error                                            { % parseError []    }
        -- TODO
        --     { failwith
        --        (Printf.sprintf "parse error near characters %d-%d"
        --          (Parsing.symbol_start ())
        --          (Parsing.symbol_end ())) }

SimpleExpr :: { Expr }
    : '(' Expr ')'                { $2         }
    | '(' ')'                     { EUnit      }
    | bool                        { EBool $1   }
    | int                         { EInt $1    }
    | float                       { EFloat $1  }
    | ID                          { EVar $1    }
    | SimpleExpr '.' '(' Expr ')' { EGet $1 $4 }

ID :: { Id }
    : id   { $1 }
    | wild {% genTmp TUnit }

FunDef :: { EFunDef }
    : ID FormalArgs '=' Expr {% efundef $1 $2 $4 }

FormalArgs :: { [(Id,Type)] }
    : ID FormalArgs {% fmap (:$2)     (addType $1) }
    | ID            {% fmap singleton (addType $1) }

ActualArgs :: { [Expr] }
    : ActualArgs SimpleExpr %prec prec_app { $1 ++ [$2] }
    | SimpleExpr            %prec prec_app { [$1] }

Elems :: { [Expr] }
    : Elems ',' Expr { $1 ++ [$3] }
    | Expr ',' Expr  { [$1,$3]}

Pat :: { [(Id,Type)] }
    : Pat ',' ID {% do x1 <- addType $3; return ($1++[x1]) }
    | ID ',' ID  {% do x1 <- addType $1; x2 <- addType $3; return [x1,x2] }

{

parseError :: [Token] -> Caml a
parseError tks = throw $ Failure "Parse Error"

addType :: Id -> Caml (Id, Type)
addType x = do
    ty <- genType
    return (x,ty)

neg :: Expr -> Expr
neg (EFloat f) = EFloat (-f)
neg e = ENeg e

elet :: Id -> Expr -> Expr -> Caml Expr
elet x e1 e2 = do
    ty <- genType
    return $ ELet (x,ty) e1 e2

eseq :: Expr -> Expr -> Caml Expr
eseq e1 e2 = do
    s <- genTmp TUnit
    return $ ELet (s,TUnit) e1 e2

efundef :: Id -> [(Id,Type)] -> Expr -> Caml EFunDef
efundef x args body = do
    ty <- genType
    return $ EFunDef (x,ty) args body

singleton :: a -> [a]
singleton x = [x]

}



{
module Language.Ptlish.Parser (parsePtlish, stringToPtlish) where
import Language.Ptlish.Lexer
import Language.Ptlish.AST
import System.IO
import Data.Maybe
import Data.Typeable
import Prelude hiding (catch) 
import Control.Exception hiding (Handler)
import System.Exit
import Control.Monad.State
import qualified Data.Map  as Map
}

%name parsePtlish
%tokentype { Token }
%error { parseError }

%token
    lparen { Tk _ TLParen  }
    rparen { Tk _ TRParen }
    not { Tk _ TNot }
    abs { Tk _ TAbs }
    old { Tk _ TOld }
    tilde { Tk _  TTilde }
    and { Tk _  TAnd }
    or { Tk _ TOr}
    lt { Tk _ TLt}
    gt { Tk _ TGt}
    le { Tk _ TLe}
    ge { Tk _ TGe}
    equals { Tk _ TEquals}
    ne { Tk _ TNe}
    rarrow { Tk _ TRArrow}
    plus { Tk _ TPlus}
    minus { Tk _ TMinus}
    pipe { Tk _ TPipe}
    amp { Tk _ TAmp}
    colon { Tk _ TColon}
    semicolon { Tk _ TSemicolon}
    def { Tk _ TDef}
    shr { Tk _ TShr}
    shl { Tk _ TShl}
    caret { Tk _ TCaret}
    asterisk { Tk _ TAsterisk }
    slash { Tk _ TSlash }
    mod { Tk _ TMod  }
    int { Tk _ (TInt $$) }
    when { Tk _ TWhen }
    id { Tk _ (TId $$) }
    backslash { Tk _ TBackslash }
    comma { Tk _ TComma }
%left when   
%left and or
%left lt gt le ge equals ne
%left shl shr amp pipe caret
%left plus minus
%left asterisk slash mod

%monad { ParseM }

%%

toplevel: statements { reverse $1 }
statements: { [] }
          | statements stmt { $2 : $1 }
stmt: id def expr semicolon 
    {% do
        s <- get
        if $1 `Map.member` (head s)
            then fail ($1 ++ " already defined")
            else do  
                put ((Map.insert $1 (NamedExpr $3) (head s)):s)
                return (Def $1 $3) 
    }
    | id actions semicolon { Trigger $1 (reverse $2) }

actions: action { [$1]}
       | actions action { $2 : $1 }

action: rarrow id exprs { Action $2 (reverse $3) } 

exprs: expr { [$1] }
     | exprs comma expr { $3 : $1 }

expr: not expr { UnExpr Not $2 }
    | abs expr { UnExpr Abs $2 }
    | old expr { UnExpr Old $2 }
    | tilde expr { UnExpr BitNot $2 }
    | expr and expr { BinExpr And $1 $3 }
    | expr or expr { BinExpr Or $1 $3 }
    | expr lt expr { BinExpr Lt $1 $3 }
    | expr gt expr { BinExpr Gt $1 $3 }
    | expr le expr { BinExpr Le $1 $3 }
    | expr ge expr { BinExpr Ge $1 $3 }
    | expr equals expr { BinExpr Eq $1 $3 }
    | expr ne expr { BinExpr Ne $1 $3 }
    | expr plus expr { BinExpr Add $1 $3 }
    | expr minus expr { BinExpr Sub $1 $3 }
    | expr asterisk expr { BinExpr Mul $1 $3 }
    | expr slash expr { BinExpr Div $1 $3 }
    | expr mod expr { BinExpr Mod $1 $3 }
    | expr amp expr { BinExpr BitAnd $1 $3 }
    | expr pipe expr { BinExpr BitOr $1 $3 }
    | expr shl expr { BinExpr BitShl $1 $3 }
    | expr shr expr { BinExpr BitShr $1 $3 }
    | expr caret expr { BinExpr BitXor $1 $3 }
    | expr when expr { BinExpr When $1 $3 }
    | id colon maybeExpr { VarExpr $1 $3 }
    | id {% do
        (x:_) <- get
        case Map.lookup $1 x of
            Just (NamedExpr _) -> return (RefExpr $1)
            Just Param -> return (UnboundExpr $1)
            Nothing -> fail ("Undeclared identifier : " ++ $1)
        }
    | lparen expr rparen { $2 }
    | expr expr  { ApplyExpr $1 $2 }
    | lambdaDef expr  
        {% do
            xs <- get
            put (tail xs)
            return (LambdaExpr $1 $2) }
    | int { ConstExpr $1 }

maybeExpr: expr { $1 }
         | { ConstExpr 0 }

lambdaDef: backslash id rarrow {% do
    prev <- get
    put ((Map.insert $2 Param (head prev)):prev)
    return $2        
           }
                
{
data ParseError = ParseError String deriving (Show, Typeable)
instance Exception ParseError

type ParseM = State [Scope]
type Scope = Map.Map Name ScopeElem

data ScopeElem = NamedExpr Expr
               | Param
              

parseError :: [Token] -> a
parseError (t:ts) = throw (ParseError $ "Parse error : unexpected " ++ show (tokenType t) ++ " at line " ++ show (tokenLineNum t) ++ " col " ++ show (tokenColNum t))
parseError _ = throw (ParseError $ "Parse error : unexpected end of file")

initParseState :: [Scope]
initParseState = [Map.empty]

stringToPtlish :: String -> Ptlish 
stringToPtlish s =  evalState (parsePtlish $! lexer s) initParseState
}

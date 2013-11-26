{
module Language.Ptlish.Lexer (lexer, tokenType, tokenLineNum, tokenColNum, Token(..), TokenType(..) ) where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
tokens :-
	$white+	;
	"--".*	;
    \{\-.*\-\} ;
    \n ;
    \( { mkT TLParen }
    \) { mkT TRParen }
    "not" { mkT TNot }
    "abs" { mkT TAbs }
    "old" { mkT TOld }
    \~ { mkT TTilde }
    "and" { mkT TAnd }
    "or" { mkT TOr }
    \< { mkT TLt }
    \> { mkT TGt }
    \<\= { mkT TLe }
    \>\= { mkT TGe }
    \= { mkT TEquals }
    \/\= { mkT TNe }
    \-\> { mkT TRArrow }
    \+ { mkT TPlus }
    \- { mkT TMinus }
    \| { mkT TPipe }
    \& { mkT TAmp }
    \: { mkT TColon }
    \; { mkT TSemicolon }
    \:\= { mkT TDef }
    \>\> { mkT TShr }
    \<\< { mkT TShl }
    \^ { mkT TCaret }
    \* { mkT TAsterisk }
    \/ { mkT TSlash }
    \\ { mkT TBackslash }
    "mod" { mkT TMod }
    "when" { mkT TWhen }
    (\-|"") $digit+ { mkTvar (TInt . read) }
    [ $alpha \_ ][ $alpha \. \_ $digit \? ]*  { mkTvar TId  }
{

data Token = Tk AlexPosn TokenType deriving (Show)
data TokenType = TLParen 
    | TRParen
    | TNot
    | TAbs
    | TOld
    | TTilde
    | TAnd
    | TOr
    | TLt
    | TGt
    | TLe
    | TGe
    | TEquals
    | TNe
    | TRArrow
    | TPlus
    | TMinus
    | TPipe
    | TAmp
    | TColon
    | TSemicolon
    | TDef
    | TShr
    | TShl
    | TCaret
    | TAsterisk
    | TSlash
    | TBackslash
    | TMod
    | TWhen
    | TInt Int
    | TId String
    deriving (Show)

mkT :: TokenType -> AlexPosn -> String -> Token
mkT t p s = Tk p t

mkTvar :: (String -> TokenType) -> AlexPosn -> String -> Token
mkTvar st p s = Tk p (st s)

tokenLineNum (Tk p _) = getLineNum p
tokenColNum (Tk p _)  = getColNum p
tokenType (Tk _ t) = t

getLineNum :: AlexPosn -> Int
getLineNum (AlexPn offset lineNum colNum) = lineNum

getColNum :: AlexPosn -> Int
getColNum (AlexPn offset lineNum colNum) = colNum

lexer = alexScanTokens 
}

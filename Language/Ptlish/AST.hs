module Language.Ptlish.AST where
import Data.List

type Ptlish = [Stmt]
type Name = String

data Stmt = Def Name Expr
          | Trigger Name [Action]
          deriving (Show)

data Expr = UnExpr UnOp Expr
          | BinExpr BinOp Expr Expr
          | ConstExpr Int
          | IfExpr Expr Expr Expr
          | VarExpr Name Expr
          | RefExpr Name
          | UnboundExpr Name
          | ApplyExpr Expr Expr
          | LambdaExpr Name Expr
          deriving (Eq, Ord)
sexp :: Show a => String -> [a] -> String
sexp a bs = "(" ++ a ++ " " ++ (intercalate " " $ map show bs) ++ ")"

instance Show Expr where
    show (UnExpr op e) = sexp (show op) [e]
    show (BinExpr op e1 e2) = sexp (show op) [e1,e2]
    show (ConstExpr c) = show c
    show (IfExpr i t e) = sexp "if" [i,t,e]
    show (VarExpr v e) = v ++ ":" ++ show e
    show (RefExpr n) = n
    show (UnboundExpr n)=n
    show (ApplyExpr e1 e2) = sexp (show e1) [e2]
    show (LambdaExpr n e) = sexp ("lambda " ++ n) [e]

data UnOp = Not | Abs | Old | BitNot
          deriving (Eq, Ord)
instance Show UnOp where
    show Not = "not"
    show Abs = "abs"
    show Old = "old"
    show BitNot = "~"

    
data BinOp = And | Or | Lt | Gt | Le | Eq | Ne | Ge | Add | Sub
           | Mul | Div | Mod | BitAnd | BitOr | BitShl | BitShr | BitXor
           | When
           deriving (Eq, Ord) 

instance Show BinOp where
    show op = case op of
        And -> "and"
        Or  -> "or"
        Lt  -> "<"
        Gt  -> ">"
        Le  -> "<="
        Eq  -> "=="
        Ne  -> "!="
        Ge  -> ">="
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        Div -> "/"
        Mod -> "%"
        BitAnd -> "&"
        BitOr -> "|"
        BitShl -> "<<"
        BitShr -> ">>"
        BitXor -> "^"
        When -> "was"

data Action = Action Name [Expr] deriving (Show)


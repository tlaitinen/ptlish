module Language.Ptlish.AST where

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
          deriving (Show)
          
data UnOp = Not | Abs | Old | BitNot
          deriving (Show)
data BinOp = And | Or | Lt | Gt | Le | Eq | Ne | Ge | Add | Sub
           | Mul | Div | Mod | BitAnd | BitOr | BitShl | BitShr | BitXor
           deriving (Show) 

data Action = Action Name [Expr] deriving (Show)


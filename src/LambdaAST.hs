
module LambdaAST where


data Expr
    = Var String          
    | Abs String Expr
    | App Expr Expr
    deriving (Show, Eq)

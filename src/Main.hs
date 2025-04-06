module Main where

import LambdaAST
import Parser
import System.IO


betaReduce :: String -> Expr -> Expr -> Expr
betaReduce toSub sub expr = 
        case expr of
    Var x            -> if x == toSub then sub else Var x
    Abs x funcexpr   -> if x == toSub then Abs x funcexpr else Abs x (betaReduce toSub sub funcexpr)
    App arg1 arg2    -> App (betaReduce toSub sub arg1) (betaReduce toSub sub arg2)


eval :: Expr -> Expr
eval (App (Abs toSub funcexpr) freevar) = eval (betaReduce toSub freevar funcexpr)
eval (App arg1 arg2)                = App (eval arg1) (eval arg2)
eval (Abs toSub funcexpr)           = Abs toSub (eval funcexpr)
eval x                              = x

-- alphaconv :: Expr -> Expr

-- step :: 



-- subscript

main :: IO ()
main = do
    putStrLn "λ> "
    input <- getLine
    case topParser input of
        Left err -> print err
        Right ast -> do
            print ast
            print (eval ast)

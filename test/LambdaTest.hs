{-# LANGUAGE FlexibleInstances #-}

module LambdaTest where 

import LambdaAST
import Evaluator
import Parser (topParser)
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import Data.Foldable (toList)
import Data.Maybe (isNothing)

-- Property: eval is idempotent
idempotentEval :: Lambda String -> Bool
idempotentEval expr = eval (eval expr) == eval expr

freeVariablesPreserved :: Lambda String -> Bool
freeVariablesPreserved expr =
  let freeVars (Var x)     = [x]
      freeVars (App t u)   = freeVars t ++ freeVars u
      freeVars (LamAbs _ b) = freeVars' b
      freeVars' = toList >>= (\v -> case v of Free x -> [x]; Bound -> [])
  in case step expr of
      Just e' -> freeVars expr == freeVars e'
      Nothing -> True


-- Test suite
tests :: TestTree 
tests = testGroup "Lambda Calculus Properties"
  [ QC.testProperty "Idempotent Evaluation" idempotentEval
  , QC.testProperty "Free Variables Preserved" freeVariablesPreserved
  ]

-- Use `lam` for scoping
instance Arbitrary (Lambda String) where
  arbitrary = sized genExpr

genExpr :: Int -> Gen (Lambda String)
genExpr 0 = Var <$> elements ["x", "y", "z"]
genExpr n = oneof
  [ Var <$> elements ["x", "y", "z"]
  , App <$> genExpr (n `div` 2) <*> genExpr (n `div` 2)
  , do
      x <- elements ["x", "y", "z"]
      body <- genExpr (n `div` 2)
      return $ lam x body  
  ]

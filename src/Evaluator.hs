-- |
-- Module      : Evaluator
-- Description : Implements beta-reduction, single-step evaluation, and normalization
-- for lambda calculus expressions. This module defines the evaluation semantics
-- for the Lambda AST using monadic substitution.

module Evaluator where

import LambdaAST

-- | Performs β-reduction by substituting the argument expression
-- for the bound variable in the body of a lambda abstraction.
-- 
-- The body must be of type @Lambda (Variable a)@, where bound variables
-- are marked with @Bound@ and free variables with @Free a@.
--
-- The argument is substituted wherever @Bound@ appears.
subst :: Lambda (Variable a) -> Lambda a -> Lambda a
subst body arg = body >>= (\v -> case v of
  Bound  -> arg
  Free x -> Var x)

-- | Performs a single evaluation step of β-reduction.
--
-- If the expression is an application of a lambda abstraction to an argument,
-- it reduces by substitution. Otherwise, it tries to reduce either the function
-- or the argument recursively.
--
-- Returns @Nothing@ if the expression is in normal form.
step :: Lambda a -> Maybe (Lambda a)
step (App (LamAbs _ body) arg) = Just (subst body arg)
step (App f x) =
  case step f of
    Just f' -> Just (App f' x)
    Nothing ->
      case step x of
        Just x' -> Just (App f x')
        Nothing -> Nothing
step (LamAbs x body) = LamAbs x <$> step body
step _ = Nothing

-- | Produces a list of successive evaluation steps from the input expression
-- to its normal form.
--
-- Each element in the list represents the expression at a stage of evaluation.
-- The final element is the expression in normal form.
traceSteps :: Lambda a -> [Lambda a]
traceSteps expr = case step expr of
  Just next -> expr : traceSteps next
  Nothing   -> [expr]

-- | Normalizes a lambda expression by repeatedly applying 'step'
-- until no further reductions can be made.
--
-- Returns the expression in normal form.
eval :: Lambda a -> Lambda a
eval expr = case step expr of
  Just next -> eval next
  Nothing   -> expr

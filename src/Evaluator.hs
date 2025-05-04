module Evaluator where

import LambdaAST


-- β-reduction 
subst :: Lambda (Variable a) -> Lambda a -> Lambda a
subst body arg = body >>= (\v ->  case v of
  Bound  -> arg
  Free x -> Var x)

-- single step evaluation 
step :: Lambda a -> Maybe (Lambda a)
step (App (LamAbs _ body) arg) = Just (subst body arg)
step (App f x) =
  case step f of
    Just f' -> Just (App f' x)
    Nothing -> App f <$> step x
step (LamAbs x body) = LamAbs x <$> step body
step _ = Nothing

-- normalization 
eval :: Lambda a -> Lambda a
eval expr = case step expr of
  Just next -> eval next
  Nothing   -> expr

  -- One evaluation step with flag indicating progress
evalStep :: Lambda a -> (Lambda a, Bool)
evalStep expr =
  case step expr of
    Just next -> (next, True)
    Nothing   -> (expr, False)


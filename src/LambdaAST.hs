{-# LANGUAGE DeriveTraversable #-}
module LambdaAST where

import Control.Monad (ap)
import Prettyprinter
import Prettyprinter.Render.String (renderString)

-- Variable type with bound and free variables
data Variable a = Bound | Free a
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- Lambda calculus term structure
data Lambda var
    = Var var
    | App (Lambda var) (Lambda var)
    | LamAbs String (Lambda (Variable var))
    deriving (Show, Eq, Functor, Foldable, Traversable)

instance Applicative Variable where
  pure = Free
  Free f <*> Free x = Free (f x)
  Bound <*> _ = Bound
  _ <*> Bound = Bound

instance Monad Variable where
  Free a >>= f = f a
  Bound  >>= _ = Bound

instance Applicative Lambda where
  pure = Var
  (<*>) = ap

instance Monad Lambda where
  Var a       >>= f = f a
  App t u     >>= f = App (t >>= f) (u >>= f)
  (LamAbs i t) >>= f = LamAbs i $ t >>= mapM f

lam :: String -> Lambda String -> Lambda String
lam x t = LamAbs x (s <$> t) where
   s x' = if x' == x then Bound else (Free x')

prettyLambda :: (a -> Doc ann) -> Lambda a -> Doc ann
prettyLambda p (Var a) = p a
prettyLambda p (App t u) = parens (prettyLambda p t <+> prettyLambda p u)
prettyLambda p (LamAbs x t) =
  group $ pretty "λ" <> pretty x <> pretty "." <+> prettyLambda p' t
  where
    p' Bound     = pretty x
    p' (Free x') = p x'

renderLambda :: Lambda String -> String
renderLambda = renderString . layoutPretty defaultLayoutOptions . prettyLambda pretty 


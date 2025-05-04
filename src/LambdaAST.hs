{-# LANGUAGE DeriveTraversable #-}

-- |
-- Module      : LambdaAST
-- Description : Abstract syntax tree and core data types for lambda calculus.
-- 
-- This module defines the representation of lambda calculus expressions, including
-- support for distinguishing free and bound variables, monadic substitution, and
-- pretty-printing using the 'prettyprinter' library.

module LambdaAST where

import Control.Monad (ap)
import Prettyprinter
import Prettyprinter.Render.String (renderString)

-- | A variable in a lambda expression, either bound or free.
--
-- 'Bound' represents a variable that is bound by a lambda abstraction.
-- 'Free a' represents a free variable identified by some name or value of type 'a'.
data Variable a = Bound | Free a
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | The abstract syntax tree for lambda calculus terms.
--
-- Parameterized by the type of variables (e.g., 'String' or 'Variable a').
--
-- Constructors:
--
-- * 'Var' — A variable, either bound or free.
-- * 'App' — Function application: @(f x)@
-- * 'LamAbs' — Lambda abstraction: @λx. body@
--
-- In 'LamAbs', the bound variable is named (for pretty-printing), but the body uses
-- 'Variable var' to distinguish bound from free variables.
data Lambda var
    = Var var
    | App (Lambda var) (Lambda var)
    | LamAbs String (Lambda (Variable var))
    deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Applicative instance for 'Variable'.
--
-- Allows the use of <*> to apply functions inside 'Free', propagating 'Bound'.
instance Applicative Variable where
  pure = Free
  Free f <*> Free x = Free (f x)
  Bound <*> _ = Bound
  _ <*> Bound = Bound

-- | Monad instance for 'Variable'.
--
-- Enables substitution: 'Free x' is replaced by a function result,
-- 'Bound' remains unchanged.
instance Monad Variable where
  Free a >>= f = f a
  Bound  >>= _ = Bound

-- | Applicative instance for 'Lambda'.
--
-- Enables structured application over 'Lambda' expressions.
instance Applicative Lambda where
  pure = Var
  (<*>) = ap

-- | Monad instance for 'Lambda'.
--
-- This is the heart of substitution:
-- - 'Var a' is substituted by applying the function.
-- - 'App' and 'LamAbs' are recursively traversed.
-- - For 'LamAbs', the body is mapped with 'mapM f', because it contains nested 'Variable var' terms.
instance Monad Lambda where
  Var a       >>= f = f a
  App t u     >>= f = App (t >>= f) (u >>= f)
  (LamAbs i t) >>= f = LamAbs i $ t >>= mapM f

-- | Smart constructor for lambda abstractions.
--
-- Takes a variable name and a body term with plain 'String' variables,
-- and returns a 'Lambda' expression where the named variable is replaced
-- with 'Bound' inside the body, and all other variables become 'Free'.
--
-- This is what transforms a 'Lambda String' into a scoped 'Lambda (Variable String)'.
lam :: String -> Lambda String -> Lambda String
lam x t = LamAbs x (s <$> t)
  where
    s x' = if x' == x then Bound else Free x'

-- | Pretty-prints a lambda expression using a user-supplied printer for variables.
--
-- Free variables are printed via the given function @p :: a -> Doc ann@.
-- Bound variables are printed by the variable name captured in the nearest 'LamAbs'.
prettyLambda :: (a -> Doc ann) -> Lambda a -> Doc ann
prettyLambda p (Var a) = p a
prettyLambda p (App t u) = parens (prettyLambda p t <+> prettyLambda p u)
prettyLambda p (LamAbs x t) =
  group $ pretty "λ" <> pretty x <> pretty "." <+> prettyLambda p' t
  where
    -- Restores name 'x' for Bound variables, delegates to 'p' for Free ones
    p' Bound     = pretty x
    p' (Free x') = p x'

-- | Renders a lambda expression as a human-readable string using 'prettyLambda'.
--
-- This is the default printer that uses 'String' variables.
renderLambda :: Lambda String -> String
renderLambda = renderString . layoutPretty defaultLayoutOptions . prettyLambda pretty

-- |
-- Module      : Main
-- Description : Interactive REPL for the Lambda Calculus Interpreter
--
-- This module provides a Read-Eval-Print Loop (REPL) for interacting
-- with lambda calculus expressions. Users can enter lambda terms,
-- evaluate them fully, reduce them step-by-step, or trace all reductions.
--
-- It uses the parser from `Parser`, evaluation from `Evaluator`, and
-- pretty-printing from `LambdaAST`.

module Main where

import Evaluator
import LambdaAST
import Text.Megaparsec (errorBundlePretty)
import Parser (topParser)
import System.IO (hFlush, stdout)
import Control.Monad (when)

-- | Interactive step-by-step evaluator.
--
-- Displays each intermediate expression resulting from β-reduction
-- and waits for the user to press ENTER before continuing. The user
-- may type ':q' to quit at any step.
--
-- Uses numbered steps for clarity.
stepLoop :: Lambda String -> IO ()
stepLoop expr = go 1 expr
  where
    go :: Int -> Lambda String -> IO ()
    go n e = do
      putStrLn $ "Step " ++ show n ++ ": " ++ renderLambda e
      case step e of
        Just e' -> do
          putStr "⏎ to continue, :q to quit> "
          hFlush stdout
          input <- getLine
          if input == ":q"
            then putStrLn "Stopping."
            else go (n + 1) e'
        Nothing -> putStrLn "(Expression is in normal form)"

-- | The main REPL loop.
--
-- Reads user input and dispatches commands such as:
--   - ':q'     — Quit
--   - ':h'     — Help
--   - ':sam'   — Show sample expressions
--   - ':eval'  — Evaluate to normal form
--   - ':step'  — Step-by-step evaluation
--   - ':trace' — Show all reduction steps
--
-- For any other input, attempts to parse and normalize the expression.
repl :: IO ()
repl = do
  putStr "λ> "
  hFlush stdout
  input <- getLine
  case words input of
    [":q"] -> putStrLn "Goodbye!"

    [":sam"] -> do
      putStrLn "Sample expressions:"
      putStrLn "  λx.x                  - Identity function"
      putStrLn "  (λx.x y)              - Apply identity to y"
      putStrLn "  (λx.λy.x)             - Constant function"
      putStrLn "  ((λx.λy.x) a b)       - Apply constant to a and b"
      putStrLn "  (λf.(λx.(f (f x))))   - Church numeral 2"
      repl

    [":h"] -> do
      putStrLn "Commands:"
      putStrLn "  :q     - Quit"
      putStrLn "  :h     - Show help"
      putStrLn "  :step  - Evaluate expression one step at a time"
      putStrLn "  :eval  - Evaluate expression to normal form"
      putStrLn "  :sam   - Show sample expressions"
      putStrLn "  :trace - Show all evaluation steps"
      repl

    (":eval" : rest) -> do
      let expr = unwords rest
      case topParser expr of
        Left err     -> putStrLn (errorBundlePretty err)
        Right parsed -> putStrLn $ renderLambda (eval parsed)
      repl

    (":step" : rest) -> do
      let expr = unwords rest
      case topParser expr of
        Left err     -> putStrLn (errorBundlePretty err) >> repl
        Right parsed -> stepLoop parsed
      repl

    (":trace" : rest) -> do
      let expr = unwords rest
      case topParser expr of
        Left err -> putStrLn (errorBundlePretty err)
        Right parsed -> do
          let steps = traceSteps parsed
          mapM_ (\(n, e) -> putStrLn $ "Step " ++ show n ++ ": " ++ renderLambda e)
                (zip [1..] steps)
      repl

    _ -> do
      case topParser input of
        Left err -> putStrLn $ errorBundlePretty err
        Right parsed -> putStrLn $ renderLambda (eval parsed)
      repl

-- | Entry point for the lambda calculus interpreter.
--
-- Initializes the REPL and displays a welcome message.
main :: IO ()
main = do
  putStrLn "Lambda Calculus REPL — type :h for help"
  repl

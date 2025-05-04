module Main where

import Evaluator
import LambdaAST
import Text.Megaparsec (errorBundlePretty)
import Parser (topParser)
import System.IO (hFlush, stdout)


repl :: IO ()
repl = do
  putStr "λ> "
  hFlush stdout
  input <- getLine
  case input of
    ":q" -> putStrLn "Goodbye!"
    ":sam" -> do
      putStrLn "Sample expressions:"
      putStrLn "  λ> λx.x                  - Identity function"
      putStrLn "  λ> (λx.x y)              - Apply identity to y"
      putStrLn "  λ> (λx.λy.x)             - Constant function"
      putStrLn "  λ> ((λx.λy.x) a b)       - Apply constant to a and b"
      putStrLn "  λ> (λf.(λx.(f (f x))))   - Church numeral 2"
      repl
    ":h" -> do
      putStrLn "Commands:"
      putStrLn "  :q     - Quit"
      putStrLn "  :h     - Show help"
      putStrLn "  :step  - Evaluate expression step by step"
      putStrLn "  :eval  - Evaluate expression to normal form"
      putStrLn "  :sam   - Show sample expressions"
      repl
      
    _    -> do
      case topParser input of
        Left err -> putStrLn $ errorBundlePretty err
        Right parsed -> do
          let result = eval parsed
          putStrLn $ renderLambda result
      repl

main :: IO ()
main = do
  putStrLn "Lambda Calculus REPL — type :h for help"
  repl

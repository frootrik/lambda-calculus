module Main where
import LambdaTest (tests)
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain tests

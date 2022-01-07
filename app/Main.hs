module Main where

import           Lib                (readExpr)
import           System.Environment

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)

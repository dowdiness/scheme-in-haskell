module Main where

import           Lib                (eval, readExpr)
import           System.Environment

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

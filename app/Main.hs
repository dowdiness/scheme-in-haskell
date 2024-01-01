module Main where

import           Repl               (runOne, runRepl)
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    _ -> runOne args

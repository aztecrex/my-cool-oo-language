module Main where

import System.Environment (getArgs)
import Clutch.Parser

main :: IO ()
main = do
  fn:args <- getArgs
  source <- readFile fn
  let program = parseString source
  print program
  putStrLn ""


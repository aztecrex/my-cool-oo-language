module Main where

import ParseWhile

main :: IO ()
main = do
  let source = "skip;skip"
  let program = parseString source
  print program
  putStrLn ""

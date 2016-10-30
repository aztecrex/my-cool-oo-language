import Clutch.Parser

main :: IO ()
main = do
  source <- readFile "test/test.cl"
  print $ parseString source
  putStrLn ""


import Clutch.Parser

main :: IO ()
main = do
  source <- readFile "test/test.clutch"
  print $ parseString source
  putStrLn ""


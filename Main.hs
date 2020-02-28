module Main where

import           GraphParser
import           Linter
import           System.Environment
import           System.FilePath
import           System.Process
import           Utils

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "No PyTorch file specified. Exiting."
    (x:_) -> do
      y <- readFile x
      writeFile (takeFileName x) y
      _ <- readProcess "python3" ["graph_dump.py", takeBaseName x] ""
      graph <- graphRead "tmp2.txt"
      _ <- readProcess "rm" [takeFileName x] ""
      _ <- readProcess "rm" ["tmp2.txt"] ""
      let messages = detectInconsistencies graph
       in case messages of
            [] -> putStrLn "No lint warnings found."
            _  -> printMessages messages

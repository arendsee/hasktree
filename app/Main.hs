module Main (main) where

import NewickParser(parseNewick, Tree(..))
import qualified Data.Text.IO as I
import System.Environment (getArgs)
import System.Exit (die)
import Data.Text (Text)

readNewick :: String -> IO Tree
readNewick filename = do
  txt <- I.readFile filename
  case parseNewick txt of
    (Left errmsg) -> die errmsg
    (Right tree) -> return tree

findLeafs :: Tree -> [Text]
findLeafs (Leaf name) = [name]
findLeafs (Node kids _) = concatMap (findLeafs . fst) kids

main :: IO ()
main = do
  args <- getArgs
  case args of
    [newickFilename] -> do
      tree <- readNewick newickFilename
      let leafs = findLeafs tree
      mapM_ I.putStrLn leafs
    _ -> die "Please provide a newick filename as the sole argument"

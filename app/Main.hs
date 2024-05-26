{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import NewickParser(parseNewick, Tree(..))
import qualified Data.Text.IO as I
import System.Environment (getArgs)
import System.Exit (die)
import Data.Text (Text)
import Data.Maybe (fromMaybe)

readNewick :: String -> IO Tree
readNewick filename = do
  txt <- I.readFile filename
  case parseNewick txt of
    (Left errmsg) -> die (errmsg <> "\n" <> show txt)
    (Right tree) -> return tree

findLeafs :: Tree -> [Text]
findLeafs (Leaf name) = [name]
findLeafs (Node kids _) = concatMap (findLeafs . fst) kids

findLeafWithParent :: Tree -> [(Text, Text)]
findLeafWithParent (Leaf _) = []
findLeafWithParent (Node children mayParentName) = concatMap (f . fst) children where
  f :: Tree -> [(Text, Text)]
  f (Leaf leafName) = [(leafName, fromMaybe "-" mayParentName)]
  f tree = findLeafWithParent tree

main :: IO ()
main = do
  args <- getArgs
  case args of
    [newickFilename] -> do
      tree <- readNewick newickFilename
      putStrLn "leafs:"
      mapM_ I.putStrLn (findLeafs tree)
      putStrLn "parent and leaf child:"
      mapM_ (\(child, parent) -> I.putStrLn $ child <> "\t" <> parent) (findLeafWithParent tree)
    _ -> die "Please provide a newick filename as the sole argument"

{-# LANGUAGE OverloadedStrings #-}

module NewickParser
    ( parseNewick
    , newickParser
    , nameParser
    , lengthParser
    , Tree(..)
    ) where

import Data.Attoparsec.Text
import Data.Text (Text, singleton)
import Control.Applicative ((<|>))

-- Define a data type to represent the Newick tree structure
data Tree
  = Leaf Text
  | Node [(Tree, Maybe Double)] (Maybe Text)
  deriving (Show, Ord, Eq)

parseNewick :: Text -> Either String Tree
parseNewick txt =
  case parse newickParser txt of
    (Fail _ context errMsg) -> Left $ unlines (errMsg : context)
    (Partial _) -> Left "Incomplete tree provided"
    (Done _ tree) -> Right tree

newickParser :: Parser Tree
newickParser = do
  (tree, _) <- treeParser 
  _ <- char ';'
  return tree

treeParser :: Parser (Tree, Maybe Double)
treeParser = leafParser <|> nodeParser

leafParser :: Parser (Tree, Maybe Double)
leafParser = do
  leaf <- nameParser
  mayLength <- optional lengthParser
  return (Leaf leaf, mayLength)

-- ((A:0.1,B:0.2,(C:0.3,D:0.4)E:0.5)F:0.6,G:0.7)H:0.8;
nodeParser :: Parser (Tree, Maybe Double)
nodeParser = do
  _ <- char '('
  children <- sepBy1 treeParser (char ',')
  _ <- char ')'
  mayName <- optional nameParser
  mayLength <- optional lengthParser
  return (Node children mayName, mayLength)
  
nameParser :: Parser Text
nameParser = quotedName '"' <|> quotedName '\'' <|> unquotedName where
  unquotedName :: Parser Text
  unquotedName = takeWhile1 (notInClass ",():;'\"")

  quotedName :: Char -> Parser Text
  quotedName quoteChar = do
    _ <- char quoteChar
    name <- foldl1 (<>) <$> many1 (unescapedChar quoteChar <|> escapedChar)
    _ <- char quoteChar
    return name

  unescapedChar :: Char -> Parser Text
  unescapedChar quoteChar = takeWhile1 (\x -> x /= '\\' && x /= quoteChar)

  escapedChar :: Parser Text
  escapedChar = do
    c <- char '\\' >> anyChar
    return $ singleton c

lengthParser :: Parser Double
lengthParser = ":" >> double

-- This parser always succeeds, even when no input is consumed. This will lead
-- to an infinite loop for non-matching types when used with repeated parsers
-- such as `many`.
optional :: Parser a -> Parser (Maybe a)
optional p = option Nothing (Just <$> p)

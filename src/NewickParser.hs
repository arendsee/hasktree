{-# LANGUAGE OverloadedStrings #-}

module NewickParser
    ( parseNewick
    , newickParser
    , nameParser
    , lengthParser
    , Tree(..)
    ) where

import Data.Attoparsec.Text
import Data.Text (Text, singleton, stripEnd)
import Control.Applicative ((<|>))

-- Define a data type to represent the Newick tree structure
data Tree
  = Leaf Text
  | Node [(Tree, Maybe Double)] (Maybe Text)
  deriving (Show, Ord, Eq)

parseNewick :: Text -> Either String Tree
parseNewick = parseOnly newickParser

-- ========== PARSERS =========================================================
-- Every parser is responsible for consuming internal space (as needed) and
-- trailing space, but NOT leading space.

charS :: Char -> Parser Char
charS c = do
  x <- char c
  _ <- skipSpace
  return x

newickParser :: Parser Tree
newickParser = do
  -- This is one case where leading space needs to be consumed
  _ <- skipSpace
  (tree, _) <- treeParser 
  _ <- charS ';'
  return tree

treeParser :: Parser (Tree, Maybe Double)
treeParser = leafParser <|> nodeParser

leafParser :: Parser (Tree, Maybe Double)
leafParser = do
  leaf <- nameParser
  mayLength <- optional lengthParser
  return (Leaf leaf, mayLength)

nodeParser :: Parser (Tree, Maybe Double)
nodeParser = do
  _ <- charS '('
  children <- sepBy1 treeParser (charS ',')
  _ <- charS ')'
  mayName <- optional nameParser
  mayLength <- optional lengthParser
  return (Node children mayName, mayLength)
  
nameParser :: Parser Text
nameParser = do
  name <- quotedName '"' <|> quotedName '\'' <|> unquotedName
  _ <- skipSpace
  return name
  where
  unquotedName :: Parser Text
  unquotedName = stripEnd <$> takeWhile1 (notInClass ",():;'\"")

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
lengthParser = do
  _ <- charS ':'
  x <- double
  _ <- skipSpace
  return x

-- This parser always succeeds, even when no input is consumed. This will lead
-- to an infinite loop for non-matching types when used with repeated parsers
-- such as `many`.
optional :: Parser a -> Parser (Maybe a)
optional p = option Nothing (Just <$> p)

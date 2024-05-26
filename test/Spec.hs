{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Data.Attoparsec.Text (parseOnly, Parser)
import Data.Text (Text)
import NewickParser (newickParser, nameParser, lengthParser, Tree(..))

main :: IO ()
main = hspec spec

parseMaybe :: Parser a -> Text -> Maybe a
parseMaybe parser txt = case parseOnly parser txt of
  (Right x) -> Just x
  (Left _) -> Nothing

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

fails :: Parser a -> Text -> Expectation
fails p txt = isJust (parseMaybe p txt) `shouldBe` False

spec :: Spec
spec = do
  describe "NewickParser" $ do
    it "parses unquoted names" $ do
      parseMaybe nameParser "A" `shouldBe` Just "A"
      -- unquoted names remove all trailing space
      parseMaybe nameParser "A \t\r\n" `shouldBe` Just "A"
      parseMaybe nameParser "A.[]*&" `shouldBe` Just "A.[]*&"
      parseMaybe nameParser "小呀小苹果" `shouldBe` Just "小呀小苹果"

    it "parses quoted names" $ do
      parseMaybe nameParser "'A'" `shouldBe` Just "A"
      parseMaybe nameParser "'\"A\\''" `shouldBe` Just "\"A'"
      parseMaybe nameParser "\"A'\\\"\"" `shouldBe` Just "A'\""
      -- spaces within quotes are preserved
      parseMaybe nameParser "'A '" `shouldBe` Just "A "
      -- spaces after quotes are NOT presered
      parseMaybe nameParser "'A' \t\r\n" `shouldBe` Just "A"

    it "parses branch lengths" $ do
      parseMaybe lengthParser ":4.5" `shouldBe` Just 4.5
      -- spaces may be present after the colon and after the token
      parseMaybe lengthParser ": 4.5 " `shouldBe` Just 4.5

    it "parses full trees" $ do
      parseMaybe newickParser "A;" `shouldBe` Just (Leaf "A")
      parseMaybe newickParser "(A,B);" `shouldBe` Just (Node [(Leaf "A", Nothing), (Leaf "B", Nothing)] Nothing)
      parseMaybe newickParser "(A,B)n1;" `shouldBe` Just (Node [(Leaf "A", Nothing), (Leaf "B", Nothing)] (Just "n1"))
      parseMaybe newickParser "(A:1,B:2)n1:3;" `shouldBe` Just (Node [(Leaf "A", Just 1), (Leaf "B", Just 2)] (Just "n1"))
      parseMaybe newickParser "(A:1,B:2)n1:3;" `shouldBe` Just (Node [(Leaf "A", Just 1), (Leaf "B", Just 2)] (Just "n1"))
      parseMaybe newickParser "(草泥马:1,Unicorn:2)n1:3;" `shouldBe` Just (Node [(Leaf "草泥马", Just 1), (Leaf "Unicorn", Just 2)] (Just "n1"))
      -- spaces may be present between all nodes
      parseMaybe newickParser " ( A : 1 , B : 2 ) n1 : 3 ; " `shouldBe` Just (Node [(Leaf "A", Just 1), (Leaf "B", Just 2)] (Just "n1"))

    it "fails to parse an invalid Newick string" $ do
      fails newickParser "bad tree"
      fails newickParser "((a;"
      fails newickParser ";a"
      fails newickParser "'a;"
      fails newickParser "\"a\"b;"

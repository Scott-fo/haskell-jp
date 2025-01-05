module JSONParser where

import Data.Char (isDigit, isSpace)

data JSONValue
  = JNull
  | JBool Bool
  | JNumber Double
  | JString String
  | JArray [JSONValue]
  | JObject [(String, JSONValue)]
  deriving (Show, Eq)

newtype ParseError = ParseError String
  deriving (Show)

type ParseResult a = Either ParseError (a, String)

parseNull :: String -> ParseResult JSONValue
parseNull input =
  if take 4 input == "null"
    then Right (JNull, drop 4 input)
    else Left $ ParseError "Expected 'null'"

parseBool :: String -> ParseResult JSONValue
parseBool input
  | take 4 input == "true" = Right (JBool True, drop 4 input)
  | take 5 input == "false" = Right (JBool False, drop 4 input)
  | otherwise = Left $ ParseError "Expected 'true' or 'false'"

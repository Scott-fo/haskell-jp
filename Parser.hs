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

parseString :: String -> ParseResult JSONValue
parseString (c : cs) = parseStringInner cs ""
parseString _ = Left $ ParseError "Expected '\"'"

-- String is an [Char]
parseStringInner :: String -> String -> ParseResult JSONValue
parseStringInner [] _ = Left $ ParseError "Unterminated String"
parseStringInner ('"' : rest) acc = Right (JString (reverse acc), rest)
parseStringInner (c : cs) acc = parseStringInner cs (c : acc)

parseNumber :: String -> ParseResult JSONValue
parseNumber input =
  case span (\c -> isDigit c || c == '-') input of
    (digits, rest) ->
      case reads digits of
        [(n, "")] -> Right (JNumber n, rest)
        _ -> Left $ ParseError "Invalid number"

parseValue :: String -> ParseResult JSONValue
parseValue [] = Left $ ParseError "Unexpected end of input"
parseValue input@(c : cs) = case c of
  'n' -> parseNull input
  't' -> parseBool input
  'f' -> parseBool input
  '"' -> parseString input
  d | isDigit d -> parseNumber input
  s | isSpace s -> parseValue cs
  _ -> Left $ ParseError $ "Unexpected character: " ++ [c]

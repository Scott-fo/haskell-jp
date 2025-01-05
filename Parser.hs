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

parseArray :: String -> ParseResult JSONValue
parseArray input = do
  let input' = dropWhile isSpace input
  case input' of
    ']' : rest -> Right (JArray [], rest)
    _ -> parseArrayElement input' []

parseArrayElement :: String -> [JSONValue] -> ParseResult JSONValue
parseArrayElement input acc = do
  (value, rest) <- parseValue input
  let rest' = dropWhile isSpace rest
  case rest' of
    ']' : remaining -> Right (JArray $ reverse (value : acc), remaining)
    ',' : remaining -> parseArrayElement (dropWhile isSpace remaining) (value : acc)
    _ -> Left $ ParseError "Expected ',' or ']'"

parseObject :: String -> ParseResult JSONValue
parseObject input = do
  let input' = dropWhile isSpace input
  case input' of
    '}' : remaining -> Right (JObject [], remaining)
    _ -> parseObjectPairs input' []

parseObjectPairs :: String -> [(String, JSONValue)] -> ParseResult JSONValue
parseObjectPairs input acc = do
  result <- parseString (dropWhile isSpace input)
  case result of
    (JString key, rest) -> do
      case dropWhile isSpace rest of
        ':' : value -> do
          (value', remaining) <- parseValue (dropWhile isSpace value)
          case dropWhile isSpace remaining of
            '}' : remaining' ->
              Right (JObject (reverse ((key, value') : acc)), remaining')
            ',' : remaining' ->
              parseObjectPairs
                (dropWhile isSpace remaining')
                ((key, value') : acc)
            _ -> Left $ ParseError "Expected ',' or '}'"
        _ ->
          Left $ ParseError "Expected ':'"

parseValue :: String -> ParseResult JSONValue
parseValue [] = Left $ ParseError "Unexpected end of input"
parseValue input@(c : cs) = case c of
  'n' -> parseNull input
  't' -> parseBool input
  'f' -> parseBool input
  '"' -> parseString input
  '[' -> parseArray cs
  '{' -> parseObject cs
  d | isDigit d -> parseNumber input
  s | isSpace s -> parseValue cs
  _ -> Left $ ParseError $ "Unexpected character: " ++ [c]

parseJSON :: String -> Either ParseError JSONValue
parseJSON input = case parseValue $ dropWhile isSpace input of
  Left err -> Left err
  Right (value, rest) ->
    if all isSpace rest
      then Right value
      else Left $ ParseError "Trailing characters after JSON value"

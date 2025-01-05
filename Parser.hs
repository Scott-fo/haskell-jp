{-# LANGUAGE LambdaCase #-}

module JSONParser where

import Control.Applicative
import Control.Monad (void)
import Data.Char (isDigit, isSpace)

data JSONValue
  = JNull
  | JBool Bool
  | JNumber Double
  | JString String
  | JArray [JSONValue]
  | JObject [(String, JSONValue)]
  deriving (Show, Eq)

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, rest) <- p input
    Just (f x, rest)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  Parser p1 <*> Parser p2 = Parser $ \input -> do
    (f, rest1) <- p1 input
    (x, rest2) <- p2 rest1
    Just (f x, rest2)

instance Monad Parser where
  return = pure
  Parser p1 >>= f = Parser $ \input -> do
    (x, rest1) <- p1 input
    let Parser p2 = f x
    p2 rest1

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser p1 <|> Parser p2 = Parser $ \input ->
    p1 input <|> p2 input

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $
  \case
    c : cs | pred c -> Just (c, cs)
    _ -> Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string "" = pure ""
string (c : cs) = (:) <$> char c <*> string cs

spaces :: Parser ()
spaces = void $ many $ satisfy isSpace

jsonNull :: Parser JSONValue
jsonNull = JNull <$ string "null"

jsonBool :: Parser JSONValue
jsonBool = (JBool True <$ string "true") <|> (JBool False <$ string "false")

jsonString :: Parser JSONValue
jsonString = JString <$> (char '"' *> many (satisfy (/= '"')) <* char '"')

jsonNumber :: Parser JSONValue
jsonNumber = Parser $ \input -> case span isDigit input of
  (digits@(_ : _), rest) -> Just (JNumber (read digits), rest)
  _ -> Nothing

jsonArray :: Parser JSONValue
jsonArray = JArray <$> (char '[' *> spaces *> elements <* spaces <* char ']')
  where
    elements = sepBy jsonValue (spaces *> char ',' <* spaces)

jsonObject :: Parser JSONValue
jsonObject = JObject <$> (char '{' *> spaces *> pairs <* spaces <* char '}')
  where
    pairs = sepBy pair (spaces *> char ',' <* spaces)
    pair = do
      result <- jsonString
      case result of
        JString key -> do
          spaces *> char ':' *> spaces
          value <- jsonValue
          return (key, value)
        _ -> empty

jsonValue :: Parser JSONValue
jsonValue = spaces *> value <* spaces
  where
    value =
      jsonNull
        <|> jsonBool
        <|> jsonNumber
        <|> jsonString
        <|> jsonArray
        <|> jsonObject

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepInner p sep <|> pure []
  where
    sepInner p sep = (:) <$> p <*> many (sep *> p)

parseJSON :: String -> Maybe JSONValue
parseJSON input = case runParser jsonValue input of
  Just (value, rest) | all isSpace rest -> Just value
  _ -> Nothing

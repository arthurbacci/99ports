module Parser ( Action (..), Parser, parse, parse_args
              , ParsedInfo (..), info_parser) where

import Network.Curl
import Control.Applicative
import Data.Maybe

newtype Parser a = P (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (P f) = f

instance Functor Parser where
  fmap f x = P $ \s -> fmap (\(a, b) -> (f a, b)) $ parse x s



instance Applicative Parser where
  pure x = P $ \s -> Just (x, s)
  x <*> y = P $ \s -> parse x s >>= \(z, s) -> parse (fmap z y) s

instance Monad Parser where
  -- Parser a >>= (a -> Parser b) -> Parser b
  x >>= y = P $ \s -> parse x s >>= \(z, s) -> parse (y z) s


instance Alternative Parser where
  empty = P $ \s -> Nothing
  x <|> y = P $ \s -> parse x s <|> parse y s



-- general utilities

consume_char :: Parser Char
consume_char = P $ \s -> case s of
                              [] -> Nothing
                              (x:xs) -> Just (x, xs)

char :: (Char -> Bool) -> Parser Char
char f = P $ \s -> parse consume_char s >>= \(x, s) -> if f x
                                                          then Just (x, s)
                                                          else Nothing

consume :: Int -> Parser String
consume 0 = P $ \_ -> Nothing
consume 1 = fmap (\c -> [c]) consume_char
consume i = do x  <- consume_char
               xs <- consume $ i - 1
               return (x:xs)

string :: String -> Parser String
string str = P $ \s -> parse (consume $ length str) s >>= \(x, s) ->
  if x == str
     then Just (str, s)
     else Nothing


-- parser for args

data Action = UpdateRepository | SearchPackages | DownloadSource

instance Show Action where
  show UpdateRepository = "UpdateRepository"
  show SearchPackages   = "SearchPackages"
  show DownloadSource   = "DownloadSource"

parse_args :: Parser [(Action, [String])]
parse_args = do x <- command
                xs <- parse_args
                return (x:xs)
  <|> return []

command :: Parser (Action, [String])
command = update_repo <|> search_packages <|> download_source

update_repo :: Parser (Action, [String])
update_repo = do string "u"
                 return (UpdateRepository, [])

search_packages :: Parser (Action, [String])
search_packages = do string "s "
                     pkgs <- packages
                     return (SearchPackages, pkgs)

download_source :: Parser (Action, [String])
download_source = do string "d "
                     pkgs <- packages
                     return (DownloadSource, pkgs)

packages :: Parser [String]
packages = do x <- package
              xs <- packages
              return (x:xs)
  <|> return []

package :: Parser String
package = do string " "
             return ""
  <|> do string "/"
         c <- consume_char
         pkg <- package
         return (c:pkg)
  <|> do c <- consume_char
         pkg <- package
         return (c:pkg)

-- parser for .info

data ParsedInfo = ParsedInfo
  { name :: String
  , version :: String
  , homepage :: URLString
  , download :: [URLString]
  , checksum :: [String]
  , download_64 :: [URLString]
  , checksum_64 :: [String]
  , dependencies :: [String]
  , maintainer :: String
  , email :: String
  , path :: FilePath }

info_parser :: String -> FilePath -> String -> Either String ParsedInfo
info_parser pkg path s = 
  case parse info s of
    Nothing -> Left $ "Could not parse .info of: " ++ pkg ++ "\n"
    Just (parsed, _) -> Right $ ParsedInfo
      { name = fromMaybe pkg $ lookup "PRGNAM" parsed
      , version = fromMaybe "Unknown" $ lookup "VERSION" parsed
      , homepage = fromMaybe "" $ lookup "HOMEPAGE" parsed
      , download = words $ fromMaybe "" $ lookup "DOWNLOAD" parsed
      , checksum = words $ fromMaybe "" $ lookup "MD5SUM" parsed
      , download_64 = words $ fromMaybe "" $ lookup "DOWNLOAD_x86_64" parsed
      , checksum_64 = words $ fromMaybe "" $ lookup "MD5SUM_x86_64" parsed
      , dependencies = words $ fromMaybe "" $ lookup "REQUIRES" parsed
      , maintainer = fromMaybe "John Doe" $ lookup "MAINTAINER" parsed
      , email = fromMaybe "" $ lookup "EMAIL" parsed
      , path = path }

info :: Parser [(String, String)]
info = many pair

pair :: Parser (String, String)
pair = do k <- many $ char (/= '=')
          string "="
          v <- str
          string "\n"
          return (k, v)

str :: Parser String
str = do string "\""
         s <- str_
         string "\""
         return s

str_ :: Parser String
str_ = do x <- strc
          xs <- str_
          return $ x ++ xs
  <|> return []

strc :: Parser String
strc = do string "\\\n"
          return ""
  <|> do string "\\"
         c <- consume_char
         return (c:[])
  <|> do c <- char (/= '\"')
         return (c:[])

-- dependencies:
--   directory                                        checking if packages exist
--   wreq                                                  downloading the files
--   network-uri                              getting the filename from the link

import Data.List (intercalate, isPrefixOf)
import System.Process (callCommand)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Directory (doesDirectoryExist, listDirectory)
import Control.Monad (filterM)
import Data.ByteString.Lazy (writeFile, ByteString)
import Network.URI (URI (..), parseURI, pathSegments)
import Network.Wreq (get, responseBody, Response)
import Control.Lens ((^.))
import Data.Functor ((<&>))
import Control.Exception (try, displayException, SomeException)

import Prelude hiding (writeFile)

import Parser

directory :: String
directory = "/usr/ports/"

repository :: String
repository = "git://git.slackbuilds.org/slackbuilds.git"

-- This program was only tested in the following versions of slackbuilds:
--   14.2
ports_version :: String
ports_version = "14.2"


print_usage :: IO ()
print_usage = hPutStrLn stderr
  "<usage> ::= <commands>\n\
  \<commands> ::= <command> <commands> | <nothing>\n\
  \<command> ::= <update-repo> | <search-packages> | <download-source>\n\
  \<update-repo> ::= 'u'\n\
  \<search-packages> ::= 's' <packages>\n\
  \<download-source> ::= 'd' <packages>\n\
  \<packages> ::= <package> <packages> | <nothing>\n\
  \<package> ::= ' ' | '/' <char> <package> | <char> <package>"



get_slackbuilds :: IO ()
get_slackbuilds = do
  callCommand $ "git clone " ++ repository ++ " " ++ directory ++
                " -b '" ++ ports_version ++ "' || \
                \(cd " ++ directory ++ " && git pull; cd ..)"


contents :: IO [FilePath]
contents = do
  list <- listDirectory directory
  let not_hidden = filter (not . isPrefixOf ".") list
  let fullpath = map (directory ++) not_hidden
  filtered <- filterM doesDirectoryExist fullpath
  return filtered

in_category :: String -> IO [(FilePath, String)]
in_category pkg = do
  c <- contents
  concat <$> mapM (\dir -> do
    let path = concat [dir, "/", pkg, "/"]
    exists <- doesDirectoryExist path
    if exists
      then do
        contents <- readFile $ path ++ pkg ++ ".info"
        return [(path, contents)]
      else return []) c

check_existence :: String -> [(FilePath, String)]
                          -> Either String (FilePath, String)
check_existence pkg [] = Left $ "Package not found: " ++ pkg ++ "\n"
check_existence _ [x] = Right x
check_existence pkg x = Left $ "There are more than one package matching "
                               ++ pkg ++ ": "
                               ++ intercalate ", " (map fst x) ++ "\n"

get_info :: String -> IO (Either String ParsedInfo)
get_info pkg = in_category pkg <&> check_existence pkg
                               <&> (>>= (uncurry $ info_parser pkg))

map_pkg :: (ParsedInfo -> IO (Either String b)) -> String
                                                -> IO (Either String b)
map_pkg f = (>>= either (return . Left) f) . get_info


search_package :: ParsedInfo -> String
search_package d = "Package name: " ++ name d
                ++ "\nVersion: " ++ version d
                ++ "\nPath: " ++ path d
                ++ "\nMaintainer: " ++ maintainer d
                ++ "\nHotline: " ++ email d
                ++ "\nDependencies: " ++ intercalate ", "  (dependencies d)
                ++ "\n"

download_as :: FilePath -> String -> ParsedInfo -> IO String
download_as path link d = do
  c <- try $ get link :: IO (Either SomeException (Response ByteString))
  case c of
    Left err -> return $ "Error downloading " ++ name d
                      ++ ": " ++ displayException err ++ "\n"
    Right c -> do
      writeFile path $ c ^. responseBody
      return $ "Downloaded " ++ path ++ "\n"

download_file :: ParsedInfo -> String -> IO String
download_file d link = do
  case parseURI link of
    Nothing -> return $ "Could not parse URI of: " ++ name d ++ "\n"
    Just uri -> do
      let p = (path d) ++ (last $ pathSegments uri)
      download_as p link d

download_source :: ParsedInfo -> IO [String]
download_source d = mapM (download_file d) $ download d


execute_action :: (Action, [String]) -> IO ()
execute_action (UpdateRepository, []) = get_slackbuilds
execute_action (SearchPackages, pkgs) = (flip mapM_) pkgs $ \pkg -> do
  r <- map_pkg (return . Right . search_package) pkg
  either (hPutStrLn stderr) putStrLn r
execute_action (DownloadSource, pkgs) = (flip mapM_) pkgs $ \pkg -> do
  r <- map_pkg ((fmap Right) . download_source) pkg
  either (hPutStrLn stderr) (putStr . intercalate "\n") r


main :: IO ()
main = do
  args <- (parse parse_args . (++ " ") . intercalate " ") <$> getArgs
  case args of
    Nothing      -> print_usage
    Just ([], _) -> print_usage
    Just (x , _) -> mapM_ execute_action x

{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( someFunc
    , extractDefinitions
    , Definition (..)
    , defaultDefinition
    , extractNextPageUrlFromSource
    , validateDefinition
    , ValidationResult (..)
    , extractDefinitionPageUrls
    ) where

import           Control.Concurrent    (threadDelay)
import           Control.Monad
import           Data.Char             (isDigit)
import           Data.List             (intercalate, isInfixOf, isPrefixOf)
import           Data.List.Split
import           Data.Maybe
import           Data.String.Utils     (replace, strip)
import           Network.HTTP
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           Text.HTML.TagSoup

import           Database.HDBC
import           Database.HDBC.Sqlite3

data Definition = Definition { defid   :: Int
                             , word    :: String
                             , meaning :: String
                             , example :: String
                             , author  :: String
                             , up      :: Int
                             , down    :: Int
                             , date    :: String
                             } deriving (Show, Eq)

defaultDefinition = Definition { defid = -1
                               , word = ""
                               , meaning = ""
                               , example = ""
                               , author = ""
                               , up = -1
                               , down = -1
                               , date = ""
                               }

matchAttribute :: String -> String -> Tag String -> Bool
matchAttribute attr val tag = isTagOpen tag && fromAttrib attr tag == val

isDefPanel :: Tag String -> Bool
isDefPanel = matchAttribute "class" "def-panel"

removeCloseTags :: [Tag String] -> [Tag String]
removeCloseTags = filter (not . isTagClose)

splitDefPanels :: [Tag String] -> [[Tag String]]
splitDefPanels = tail . split (keepDelimsL $ whenElt isDefPanel)

parsePanel :: [Tag String] -> Definition
parsePanel tags = foldr fill defaultDefinition $ zip3 tags (tail tags) (tail $ tail tags)
  where
    fill (tag1, tag2, tag3) def
      | matchAttribute "class" "def-panel" tag1 = def { defid = getDefId tag1 }
      | matchAttribute "class" "word" tag1 = def { word = getText tag2 }
      | matchAttribute "class" "meaning" tag1 = def { meaning = getText tag2 }
      | matchAttribute "class" "example" tag1 = def { example = getText tag2 }
      | matchAttribute "class" "author" tag1 = def { author = getText tag2
                                                   , date = getText tag3 }
      | matchAttribute "class" "count" tag1 = fillVotes tag2 def
      | otherwise = def
          where
            fillVotes tag def = case down def of
              -1 -> def { down = val } -- Will encounter down first in folding
              _  -> def { up = val}
              where val = read (innerText [tag]) :: Int
            getDefId tag = read (fromAttrib "data-defid" tag1) :: Int
            getText tag = strip . innerText $ [tag]

-- Need to do this so that paragraphs will be within a same TagText
preprocessRawSource :: String -> String
preprocessRawSource src = replace "<br>" "" src

extractDefinitions :: String -> [Definition]
extractDefinitions src = removeEmpty records
  where
    panels = splitDefPanels $ removeCloseTags $ parseTags $ preprocessRawSource $ src
    records = map parsePanel panels
    removeEmpty = filter (not . null . word)

extractNextPageUrl:: [Tag String] -> Maybe String
extractNextPageUrl tags = case linkTag of
  [] -> Nothing
  tag:_ -> Just $ fromAttrib "href" tag
  where
    linkTag = filter (matchAttribute "rel" "next") tags

extractNextPageUrlFromSource :: String -> Maybe String
extractNextPageUrlFromSource = extractNextPageUrl . parseTags

extractDefinitionPageUrls :: String -> [String]
extractDefinitionPageUrls src = retrieveLinks $ parseTags src
  where
    retrieveLinks tags = map (fromAttrib "href") defTags
      where
        linkTags = filter (isTagOpenName "a") tags
        defTags = filter (\t -> isPrefixOf "/define.php?term=" (fromAttrib "href" t)) linkTags
        --defTags = linkTags

data ValidationResult = Valid | ContainsEmptyField | InvalidExample

validateDefinition :: Definition -> ValidationResult
validateDefinition def = case (anyFieldsEmpty def, exampleInvalid def) of
  (True, _) -> ContainsEmptyField
  (_, True) -> InvalidExample
  (_, _) -> Valid
  where
    anyFieldsEmpty def = (  (defid $ empty) == (defid $ def)
                         || (word $ empty) == (word $ def)
                         || (meaning $ empty) == (meaning $ def)
                         || (example $ empty) == (example $ def)
                         || (author $ empty) == (author $ def)
                         || (up $ empty) == (up $ def)
                         || (down $ empty) == (down $ def)
                         || (date $ empty) == (date $ def) )
      where empty = defaultDefinition
    exampleInvalid def = not $ isInfixOf (word $ def) (example $ def)

insertDefinition :: IConnection conn => conn -> Definition -> IO ()
insertDefinition conn def = do
  let query = "INSERT OR REPLACE INTO urbandictionary VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
  run conn query [ toSql $ defid def
                 , toSql $ word def
                 , toSql $ meaning def
                 , toSql $ example def
                 , toSql $ author def
                 , toSql $ up def
                 , toSql $ down def
                 , toSql $ date def
                 ]
  commit conn

insertDefinitionAll :: IConnection conn => conn -> [Definition] -> IO ()
insertDefinitionAll conn defs = mapM_ (insertDefinition conn) defs

insertDefinitionsAllSimple :: [Definition] -> IO ()
insertDefinitionsAllSimple defs = do
  conn <- connectSqlite3 dbName
  insertDefinitionAll conn defs

dbName :: String
dbName = "data.db"

createDatabase :: IO ()
createDatabase = do
  conn <- connectSqlite3 dbName
  run conn "CREATE TABLE IF NOT EXISTS urbandictionary\
            \(defid INTEGER PRIMARY KEY\
            \, word TEXT\
            \, meaning TEXT\
            \, example TEXT\
            \, author TEXT\
            \, up INTEGER\
            \, down INTEGER\
            \, date TEXT\
            \)" []
  commit conn
  disconnect conn

homeURL :: String
homeURL = "http://www.urbandictionary.com"

alphabetURL :: String -> String
alphabetURL alphabet = homeURL ++ "/browse.php?character=" ++ alphabet

openURL :: String -> IO (String)
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

runDummy :: IO ()
runDummy = do
  createDatabase
  src <- readFile "test/data/apple.html"
  let records = extractDefinitions src
  insertDefinitionsAllSimple records
  return ()

runHomePage :: IO ()
runHomePage = do
  createDatabase
  putStrLn $ "Retrieving definitions from " ++ homeURL ++ "..."
  src <- openURL homeURL
  let records = extractDefinitions src
  putStrLn $ "Retrieved " ++ show (length records) ++ " definitions"
  putStrLn $ "Newest: " ++ show (head records)
  insertDefinitionsAllSimple records
  return ()

processDefinitionPage :: String -> IO ([Definition], Maybe String)
processDefinitionPage url = do
  src <- openURL url
  let records = extractDefinitions src
  let nextUrlSuffix = extractNextPageUrlFromSource src
  insertDefinitionsAllSimple records
  return $ (records, nextUrlSuffix)

crawlFromDefinitionPage :: String -> String -> IO ()
crawlFromDefinitionPage urlRoot url = do
  createDatabase
  putStrLn $ "Crawling at definition page, now at " ++ url
  (records, nextUrlSuffix) <- processDefinitionPage url
  putStrLn $ "Got these: " ++ (show records)
  putStrLn $ "Trying to rest for a while..."
  threadDelay $ 5000000
  case nextUrlSuffix of
    Just suffix -> crawlFromDefinitionPage (urlRoot) (urlRoot ++ suffix)
    Nothing -> putStrLn $ "No more next page. Bye"

crawlFromBrowsePage :: String -> String -> IO ()
crawlFromBrowsePage urlRoot url = do
  createDatabase
  putStrLn $ "Crawling at browse page, now at " ++ url
  src <- openURL url
  let suffixes = extractDefinitionPageUrls src
  let definitionUrls = map (\s -> urlRoot ++ s) suffixes
  zipWithM_ crawlFromDefinitionPage (replicate (length suffixes) urlRoot) definitionUrls
  threadDelay $ 5000000
  let nextUrlSuffix = extractNextPageUrlFromSource src
  case nextUrlSuffix of
    Just suffix -> crawlFromBrowsePage (urlRoot) (urlRoot ++ suffix)
    Nothing -> putStrLn $ "No more next page. Bye"

crawlFromMainPage :: IO ()
crawlFromMainPage = do
  crawlFromDefinitionPage homeURL homeURL

crawlFromAlphabet :: String -> IO ()
crawlFromAlphabet alphabet = do
  let url = alphabetURL alphabet
  crawlFromBrowsePage homeURL url

-- TODO: Space, redirection (case sensitivity query term)
buildSearchUrl :: String -> String
buildSearchUrl w = homeURL ++ "/define.php?term=" ++ w

prettyPrint :: Definition -> String
prettyPrint def = intercalate "\n" makeLines
  where
    makeLines = [ "\nW O R D"
                , "-------\n"
                , word def
                , "\nM E A N I N G"
                , "-------------\n"
                , meaning def
                , "\nE X A M P L E"
                , "-------------\n"
                , example def
                , "\nA U T H O R"
                , "-----------\n"
                , author def
                , "\nV O T E S"
                , "---------\n"
                , "Up: " ++ show (up def) ++ " Down: " ++ show (down def)
                , "\nD A T E"
                , "-------\n"
                , date def
                , ""
                ]

data Flag
  = Define String
  | Crawl String
  | Help
  deriving Show

options :: [OptDescr Flag]
options =
  [ Option ['d'] ["define"] (ReqArg Define "WORD") "Look up the definition of a word"
  , Option ['c'] ["crawl"]  (ReqArg Crawl "START") "Crawler mode"
  , Option ['h'] ["help"]   (NoArg Help) "Display help"
  ]

getUsageInfo :: String
getUsageInfo = usageInfo header options
  where header = "Usage: hs-urbandict-exe [OPTION...]"

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ getUsageInfo))

main :: IO ()
main = do
  (os, ns) <- getArgs >>= compilerOpts
  --putStrLn $ "Options: " ++ show os
  --putStrLn $ "Non-options: " ++ show ns
  forM_ os $ \opt -> do
    case opt of
      Help -> do
        putStrLn $ getUsageInfo
        exitWith ExitSuccess
      Define w -> do
        putStrLn $ "Looking up the word [" ++ w ++ "]..."
        src <- openURL (buildSearchUrl w)
        let records = extractDefinitions src
        putStrLn $ prettyPrint $ head records
      Crawl s -> do
        case s of
          "home" -> do
            putStrLn $ "Crawling from main page"
            crawlFromMainPage
          _ -> do
            putStrLn $ "Crawling from alphabet [" ++ s ++ "]..."
            crawlFromAlphabet s
  return ()

--someFunc = crawlFromMainPage
--someFunc = crawlFromAlphabet "A"
someFunc = main

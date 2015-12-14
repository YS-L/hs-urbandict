{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( someFunc
    , extractDefinitions
    , Definition (..)
    , defaultDefinition
    , extractNextPageUrlFromSource
    ) where

import           Control.Concurrent    (threadDelay)
import           Data.Char             (isDigit)
import           Data.List.Split
import           Data.Maybe
import           Data.String.Utils     (replace, strip)
import           Network.HTTP
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

insertDefinition :: IConnection conn => conn -> Definition -> IO ()
insertDefinition conn def = do
  let query = "INSERT OR IGNORE INTO urbandictionary VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
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

processPage url = do
  src <- openURL url
  let records = extractDefinitions src
  let nextUrlSuffix = extractNextPageUrlFromSource src
  insertDefinitionsAllSimple records
  return $ (records, nextUrlSuffix)

crawlFromUrl urlRoot url = do
  putStrLn $ "Crawling, now at " ++ url
  (records, nextUrlSuffix) <- processPage url
  putStrLn $ "Got these: " ++ (show records)
  putStrLn $ "Trying to rest for a while..."
  threadDelay $ 5000000
  case nextUrlSuffix of
    Just suffix -> crawlFromUrl (urlRoot) (urlRoot ++ suffix)
    Nothing -> putStrLn $ "No more next page. Bye"

crawFromMainPage = do
  crawlFromUrl homeURL homeURL

someFunc = crawFromMainPage

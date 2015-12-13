{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( someFunc
    , extractDefinitions
    , Definition (..)
    , defaultDefinition
    ) where

import           Data.Char             (isDigit)
import           Data.List.Split
import           Data.Maybe
import           Data.String.Utils     (strip)
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

extractDefinitions :: String -> [Definition]
extractDefinitions src = removeEmpty records
  where
    panels = splitDefPanels $ removeCloseTags $ parseTags src
    records = map parsePanel panels
    removeEmpty = filter (not . null . word)

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

insertDefinitionAll conn defs = mapM_ (insertDefinition conn) defs

dbName = "data.db"

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

--someFunc :: IO ()
--someFunc = putStrLn "Hello World"

-- someFunc = createDatabase "test.db"

{-
someFunc = do
  conn <- connectSqlite3 "test.db"
  insertDefinition conn defaultDefinition
  return ()
-}

someFunc = do
  createDatabase
  src <- readFile "test/data/apple.html"
  let records = extractDefinitions src
  conn <- connectSqlite3 dbName
  insertDefinitionAll conn records
  return ()

module Lib
    ( someFunc
    , extractDefinitions
    , Definition (..)
    , defaultDefinition
    ) where

import           Data.Char         (isDigit)
import           Data.List.Split
import           Data.Maybe
import           Network.HTTP
import           Text.HTML.TagSoup

data Definition = Definition { defid   :: String
                             , word    :: String
                             , meaning :: String
                             , example :: String
                             , author  :: String
                             , up      :: Int
                             , down    :: Int
                             } deriving (Show, Eq)

defaultDefinition = Definition { defid = ""
                               , word = ""
                               , meaning = ""
                               , example = ""
                               , author = ""
                               , up = -1
                               , down = -1
                               }

matchAttribute :: String -> String -> Tag String -> Bool
matchAttribute attr val tag = isTagOpen tag && fromAttrib attr tag == val

isDefPanel :: Tag String -> Bool
isDefPanel = matchAttribute "class" "def-panel"

splitDefPanels :: [Tag String] -> [[Tag String]]
splitDefPanels = tail . split (keepDelimsL $ whenElt isDefPanel)

parsePanel :: [Tag String] -> Definition
parsePanel tags = foldr fill defaultDefinition $ zip tags (tail tags)
  where
    fill (tag1, tag2) def
      | matchAttribute "class" "def-panel" tag1 = def { defid = fromAttrib "data-defid" tag1 }
      | matchAttribute "class" "word" tag1 = def { word = innerText [tag2] }
      | matchAttribute "class" "meaning" tag1 = def { meaning = innerText [tag2] }
      | matchAttribute "class" "example" tag1 = def { example = innerText [tag2] }
      | matchAttribute "class" "author" tag1 = def { author = innerText [tag2] }
      | matchAttribute "class" "count" tag1 = fillVotes tag2 def
      | otherwise = def
          where
            fillVotes tag def = case down def of
              -1 -> def { down = val } -- Will encounter down first in folding
              _  -> def { up = val}
              where val = read (innerText [tag]) :: Int

extractDefinitions :: String -> [Definition]
extractDefinitions src = removeEmpty records
  where
    panels = splitDefPanels $ parseTags src
    records = map parsePanel panels
    removeEmpty = filter (not . null . word)

someFunc :: IO ()
someFunc = putStrLn "Hello World"

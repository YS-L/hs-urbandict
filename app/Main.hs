module Main where

import           Control.Monad
import           Data.List             (intercalate)
import           Data.List.Split
import           Data.String.Utils     (replace, strip)
import           System.Console.GetOpt
import           System.Environment
import           System.Exit

import           Lib

prettyPrint :: Definition -> String
prettyPrint def = intercalate "\n" $ map format makeLines
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
    wrapline s = intercalate "\n" $ collect (words s) [] 0
      where
        collect (x:xs) ys c = case (length x) + c > 65 of
          False -> collect xs (x:ys) (length x + c)
          otherwise ->  (finalize ys) : (collect xs [x] (length x))
        collect [] ys _ = [finalize ys]
        finalize ys = intercalate " " (reverse ys)
    format s = intercalate "\n" $ map wrapline paragraphs
      where
        paragraphs = splitOn "\n" (replace "\r" "" s)

data Flag
  = Define String
  | Help
  deriving (Show, Eq)

options :: [OptDescr Flag]
options =
  [ Option ['d'] ["define"] (ReqArg Define "WORD") "Look up the definition of a word"
  , Option ['h'] ["help"]   (NoArg Help) "Display help"
  ]

getUsageInfo :: String
getUsageInfo = usageInfo header options
  where header = "Usage: urbandict [OPTION...]"

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ getUsageInfo))

mainSearch :: String -> IO ()
mainSearch w = do
  putStrLn $ "Looking up the word [" ++ w ++ "]..."
  src <- openURL (buildSearchUrl w)
  let records = extractDefinitions src
  putStrLn $ prettyPrint $ head records
  --putStrLn $ show (head records)

exitWithHelp :: IO ()
exitWithHelp  = do
  putStrLn $ getUsageInfo
  exitWith ExitSuccess

main :: IO ()
main = do
  (os, ns) <- getArgs >>= compilerOpts
  -- putStrLn $ "Options: " ++ show os
  -- putStrLn $ "Non-options: " ++ show ns
  if Help `elem` os
    then exitWithHelp
    else
      forM_ os $ \opt -> do
        case opt of
          Define w -> do
            mainSearch w
            exitWith ExitSuccess
  if not . null $ ns
    then mainSearch $ head ns
    else exitWithHelp
  return ()

import           Lib
import           Test.HUnit

test1 = TestCase (do src <- readFile "test/data/apple.html"
                     let defs = extractDefinitions src
                     assertEqual "Number of definitions failed" (length defs) 7
                     let firstDef = head defs
                     assertEqual "Word should be apple" "apple" (word firstDef))

firstAppleDefinition = Definition { defid = "1582300"
                                  , word = "apple"
                                  , meaning = "\nThe alternative to the orange.\n"
                                  , example = "\nToday, I do not wish to consume an apple.  I shall seek alternative fruits.\n"
                                  , author = "Armand Banana"
                                  , up = 6359
                                  , down = 1030
                                  }

test2 = TestCase (do src <- readFile "test/data/apple.html"
                     let firstDef = head $ extractDefinitions src
                     assertEqual "First def wrong" firstAppleDefinition firstDef)

tests = TestList [ test1
                 , test2
                 ]

main :: IO Counts
main = runTestTT tests

import           Data.Maybe
import           Lib
import           Test.HUnit

test1 = TestCase (do src <- readFile "test/data/apple.html"
                     let defs = extractDefinitions src
                     assertEqual "Number of definitions failed" (length defs) 7
                     let firstDef = head defs
                     assertEqual "Word should be apple" "apple" (word firstDef))

firstAppleDefinition = Definition { defid = 1582300
                                  , word = "apple"
                                  , meaning = "The alternative to the orange."
                                  , example = "Today, I do not wish to consume an apple.  I shall seek alternative fruits."
                                  , author = "Armand Banana"
                                  , up = 6359
                                  , down = 1030
                                  , date = "January 09, 2006"
                                  }

test2 = TestCase (do src <- readFile "test/data/apple.html"
                     let firstDef = head $ extractDefinitions src
                     assertEqual "First def wrong" firstAppleDefinition firstDef)

-- Note however that in live site only suffix is extracted
testPaginate = TestCase (do src <- readFile "test/data/main.html"
                            let url = extractNextPageUrlFromSource src
                            assertEqual "Failed to extract next page url"
                                        (Just "http://www.urbandictionary.com/?page=2")
                                        url
                        )

testPaginateLast = TestCase (do src <- readFile "test/data/last.html"
                                let url = extractNextPageUrlFromSource src
                                assertEqual "Should not have extracted next page URL"
                                            (Nothing) url)

tests = TestList [ test1
                 , test2
                 , testPaginate
                 , testPaginateLast
                 ]

main :: IO Counts
main = runTestTT tests

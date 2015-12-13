import           Lib
import           Test.HUnit

test1 = TestCase (do src <- readFile "test/data/apple.html"
                     let defs = extractDefinitions src
                     assertEqual "Number of definitions failed" (length defs) 7
                     let firstDef = head defs
                     assertEqual "Word should be apple" (word firstDef) "apple")

tests = TestList [TestLabel "test1" test1]

main :: IO Counts
main = runTestTT tests

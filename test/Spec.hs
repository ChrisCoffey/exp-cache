import Spec.LRUTests (lruTests)

import Test.Tasty  (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "Data.Cache" [
    lruTests
    ]

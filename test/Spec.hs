import Spec.LRUTests (lruTests)
import Spec.MRUTests (mruTests)
import Spec.RRTests (rrSpec)

import Test.Tasty  (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "Data.Cache" [
    lruTests,
    mruTests,
    rrSpec
    ]

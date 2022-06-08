import           Test.Tasty
import           Test.Tasty.HUnit

import           Tetrimino

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit Tests"
    [ testCase "Foo" $ False @? "hello" ]

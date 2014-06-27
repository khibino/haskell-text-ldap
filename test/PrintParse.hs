
module PrintParse where

import Distribution.TestSuite
  (Test (Test), TestInstance (TestInstance), Result (Pass, Fail), Progress (Finished))
import Test.QuickCheck (Testable, Gen, choose, elements, vector, Arbitrary (..), quickCheck)

import Control.Exception (try)
import System.Random (StdGen, randomR)
import Text.LDAP.Data (List1)
import Data.List.NonEmpty ((:|))

digit :: Gen Char
digit =  choose ('0', '9')

boundInt :: Int -> Int -> Gen Int
boundInt =  curry choose

list :: Gen a -> Int -> Gen [a]
list g n = sequence [ g | i <- [1..n] ]

list1 :: Gen a -> Int -> List1 a
list1 g n = do
  i <- 1 :| [2..n]
  return g

digits :: Gen String
digits =  boundInt 1 7 >>= list digit

-- attrByte

simpleInstance :: IO Progress -> String -> Test
simpleInstance p name = Test this  where
  this = TestInstance p name [] [] (\_ _ -> Right this)

testSuite :: Testable prop => prop -> String -> Test
testSuite t = simpleInstance $ do
  e <- try $ quickCheck t
  return . Finished $ either (Fail . show) (const Pass) (e :: Either IOError ())

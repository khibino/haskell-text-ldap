module Suite (testSuite) where

import Distribution.TestSuite
  (Test (Test), TestInstance (TestInstance), Result (Pass, Fail), Progress (Finished))
import Test.QuickCheck (Testable, quickCheckResult)

import Control.Exception (try)

import Error (showIOError, qresult)


simpleInstance :: IO Progress -> String -> Test
simpleInstance p name = Test this  where
  this = TestInstance p name [] [] (\_ _ -> Right this)

testSuite :: Testable prop => prop -> String -> Test
testSuite t = simpleInstance $ do
  e <- try $ quickCheckResult t
  return . Finished . either Fail (const Pass) $ do
    qr <- showIOError e
    qresult qr

module Suite (suite) where

import Distribution.TestSuite
  (Test (Test), TestInstance (TestInstance), Result (Pass, Fail), Progress (Finished))
import Test.QuickCheck (Testable, quickCheckResult)

import Control.Exception (try)

import Error (showIOError, qcEither)


simpleInstance :: IO Progress -> String -> Test
simpleInstance p name = Test this  where
  this = TestInstance p name [] [] (\_ _ -> Right this)

suite :: Testable prop => prop -> String -> Test
suite t = simpleInstance $ do
  e <- try $ quickCheckResult t
  return . Finished . either Fail (const Pass) $ do
    qr <- showIOError e
    qcEither qr

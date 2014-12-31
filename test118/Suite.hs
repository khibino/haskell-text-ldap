module Suite (suite) where

import Distribution.TestSuite
  (Test (Test), TestInstance (TestInstance), Result (Pass, Fail, Error), Progress (Finished))
import Test.QuickCheck (Testable, quickCheckResult)

import Control.Exception (try)
import Control.Applicative ((<$>))

import Error (qcEither)


simpleInstance :: IO Progress -> String -> Test
simpleInstance p name = Test this  where
  this = TestInstance p name [] [] (\_ _ -> Right this)

suite :: Testable prop => prop -> String -> Test
suite t = simpleInstance $ do
  er <- try $ qcEither <$> quickCheckResult t
  return . Finished $ case er of
    Right (Right ()) -> Pass
    Right (Left m)   -> Fail m
    Left e           -> Error $ show (e :: IOError)

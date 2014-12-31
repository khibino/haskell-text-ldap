module Suite (suite) where

import Distribution.TestSuite
  (TestOptions (..), Options (..), ImpureTestable (..), impure,
   Test, Result (Pass, Fail, Error))
import qualified Distribution.TestSuite as TestSuite
import Test.QuickCheck (Testable, quickCheckResult)

import Control.Exception (try)
import Control.Applicative ((<$>))

import Error (qcEither)


test114 :: Testable prop => prop -> IO TestSuite.Result
test114 t = do
  er <- try (qcEither <$> quickCheckResult t)
  return $ case er of
    Right (Right ()) -> Pass
    Right (Left m)   -> Fail m
    Left e           -> Error $ show (e :: IOError)

data Suite114 t = Suite114 String t

instance Testable prop => TestOptions (Suite114 prop) where
  name (Suite114 n _) = n
  options = const []
  defaultOptions = const . return $ Options []
  check _ _ = []

instance Testable prop => ImpureTestable (Suite114 prop) where
  runM (Suite114 _ t) _ = test114 t

suite :: Testable prop => prop -> String -> Test
suite t n = impure $ Suite114 n t

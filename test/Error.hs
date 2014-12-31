module Error (qresult, showIOError) where

import Test.QuickCheck
  (Testable, Gen, Arbitrary (..), Result (Success),
   choose, oneof, frequency, elements, quickCheckResult)

qresult :: Test.QuickCheck.Result -> Either String ()
qresult =  d  where
  d (Success {}) = Right ()
  d x            = Left $ show x

showIOError :: Either IOError a -> Either String a
showIOError =  d  where
  d (Right x) = Right x
  d (Left e)  = Left $ show e

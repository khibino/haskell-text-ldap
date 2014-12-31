module Error (qcEither) where

import Test.QuickCheck (Result (Success))

qcEither :: Test.QuickCheck.Result -> Either String ()
qcEither =  d  where
  d (Success {}) = Right ()
  d x            = Left $ show x

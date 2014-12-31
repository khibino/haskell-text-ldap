module Error (qcEither, showIOError) where

import Test.QuickCheck (Result (Success))

qcEither :: Test.QuickCheck.Result -> Either String ()
qcEither =  d  where
  d (Success {}) = Right ()
  d x            = Left $ show x

showIOError :: Either IOError a -> Either String a
showIOError =  d  where
  d (Right x) = Right x
  d (Left e)  = Left $ show e


module PrintParse where

import Distribution.TestSuite
  (Test (Test), TestInstance (TestInstance), Result (Pass, Fail), Progress (Finished))
import Test.QuickCheck
  (Testable, Gen, Arbitrary (..),
   choose, oneof, elements, quickCheck)

import Control.Exception (try)
import Control.Applicative ((<$>), (<*>))
import Data.ByteString.Char8 (ByteString, pack)
import Text.LDAP.Data (AttrType (..), List1)
import Data.List.NonEmpty (NonEmpty ((:|)))


simpleInstance :: IO Progress -> String -> Test
simpleInstance p name = Test this  where
  this = TestInstance p name [] [] (\_ _ -> Right this)

testSuite :: Testable prop => prop -> String -> Test
testSuite t = simpleInstance $ do
  e <- try $ quickCheck t
  return . Finished $ either (Fail . show) (const Pass) (e :: Either IOError ())


boundInt :: Int -> Int -> Gen Int
boundInt =  curry choose

list :: Gen a -> Int -> Gen [a]
list g n = sequence [ g | _i <- [1..n] ]

sequence1 :: Monad m => NonEmpty (m a) -> m (NonEmpty a)
sequence1 (ma :| ms) = do
  x   <- ma
  xs  <- sequence ms
  return $ x :| xs

list1 :: Gen a -> Int -> Gen (List1 a)
list1 g n = sequence1 $ do
  _i <- 1 :| [2..n]
  return g

digit :: Gen Char
digit =  choose ('0', '9')

oidpe :: Gen ByteString
oidpe =  (pack <$>) $ boundInt 1 10 >>= list digit

alpha :: Gen Char
alpha =  oneof [choose ('A', 'Z'), choose ('a', 'z')]

keychar :: Gen Char
keychar =  elements $ '-' : ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']

keystr :: Gen ByteString
keystr =  (pack <$>) $ (:) <$> alpha <*> (boundInt 0 40 >>= list keychar)

attrType :: Gen AttrType
attrType = oneof
           [ AttrType <$> keystr
           , (AttrOid <$>) $ boundInt 1 8 >>= list1 oidpe
           ]

{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module PrintParse (tests) where

import Distribution.TestSuite
  (Test (Test), TestInstance (TestInstance), Result (Pass, Fail), Progress (Finished))
import Test.QuickCheck
  (Testable, Gen, Arbitrary (..), Result (Success),
   choose, oneof, frequency, elements, quickCheckResult)

import Control.Exception (try)
import Control.Applicative ((<$>), (<*>))
import Data.ByteString.Char8 (ByteString, pack)
import Text.LDAP.Data
  (AttrType (..), AttrValue (..), Attribute, Component (..),
   DN, List1)
import Text.LDAP.Printer (LdapPrinter, runLdapPrinter)
import qualified Text.LDAP.Printer as Printer
import Text.LDAP.Parser (LdapParser, runLdapParser)
import qualified Text.LDAP.Parser as Parser
import Data.List.NonEmpty (NonEmpty ((:|)))


simpleInstance :: IO Progress -> String -> Test
simpleInstance p name = Test this  where
  this = TestInstance p name [] [] (\_ _ -> Right this)

qresult :: Test.QuickCheck.Result -> Either String ()
qresult =  d  where
  d (Success {}) = Right ()
  d x            = Left $ show x

showIOError :: Either IOError a -> Either String a
showIOError =  d  where
  d (Right x) = Right x
  d (Left e)  = Left $ show e

testSuite :: Testable prop => prop -> String -> Test
testSuite t = simpleInstance $ do
  e <- try $ quickCheckResult t
  return . Finished . either Fail (const Pass) $ do
    qr <- showIOError e
    qresult qr


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
oidpe =  (pack <$>) $ choose (1, 10) >>= list digit

alpha :: Gen Char
alpha =  oneof [choose ('A', 'Z'), choose ('a', 'z')]

keychar :: Gen Char
keychar =  elements $ '-' : ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']

keystr :: Gen ByteString
keystr =  (pack <$>) $ (:) <$> alpha <*> (choose (0, 40) >>= list keychar)

bstring' :: Int -> Int -> Gen ByteString
bstring' n m = (pack <$>) $ choose (n, m) >>= list (elements ['\0'..'\255'])

bstring :: Int -> Gen ByteString
bstring =  bstring' 0


attrType :: Gen AttrType
attrType =
  oneof
  [ AttrType <$> keystr
  , AttrOid  <$> (choose (1, 8) >>= list1 oidpe)
  ]

attrValue :: Gen AttrValue
attrValue =  AttrValue <$> bstring 0x200

component :: Gen Component
component =
  frequency
  [ (1, S <$> arbitrary)
  , (3, L <$> (choose (2, 5) >>= list1 arbitrary))
  ]


isoProp :: Eq a => LdapPrinter a -> LdapParser a -> a -> Bool
isoProp pr ps a = Right a == (runLdapParser ps . runLdapPrinter pr $ a)

instance Arbitrary AttrType where
  arbitrary = attrType

instance Arbitrary AttrValue where
  arbitrary = attrValue

instance Arbitrary Component where
  arbitrary = component

instance Arbitrary DN where
  arbitrary = choose (1, 30) >>= list1 arbitrary

prop_attributeIso :: Attribute -> Bool
prop_attributeIso =  isoProp Printer.attribute Parser.attribute

prop_componentIso :: Component -> Bool
prop_componentIso =  isoProp Printer.component Parser.component

prop_dnIso :: DN -> Bool
prop_dnIso =  isoProp Printer.dn Parser.dn

prop_ldifAttrIso :: (AttrType, AttrValue) -> Bool
prop_ldifAttrIso =
  isoProp
  (Printer.ldifAttr Printer.ldifEncodeAttrValue)
  (Parser.ldifAttr  Parser.ldifDecodeAttrValue)

prop_openLdapEntryIso :: (DN, [(AttrType, AttrValue)]) -> Bool
prop_openLdapEntryIso =
  isoProp
  (Printer.openLdapEntry Printer.ldifEncodeAttrValue)
  (Parser.openLdapEntry  Parser.ldifDecodeAttrValue)

tests :: IO [Test]
tests =
  return
  [ testSuite prop_attributeIso "attribute iso - print parse"
  , testSuite prop_componentIso "component iso - print parse"
  , testSuite prop_dnIso "dn iso - print parse"
  , testSuite prop_ldifAttrIso "ldifAttr iso - print parse"
  , testSuite prop_openLdapEntryIso "openLdapEntry iso - print parse"
  ]

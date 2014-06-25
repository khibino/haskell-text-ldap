{-# LANGUAGE OverloadedStrings #-}

module Text.LDAP.Printer
       ( runLdapPrinter
       , dn
       ) where

import Prelude hiding (reverse)
import Numeric (showHex)
import Data.Monoid (mempty, (<>))
import Data.DList (DList, toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Char (chr, isPrint)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (ByteString, singleton)
import qualified Data.ByteString.Lazy as LB
import Control.Applicative (pure)
import Control.Monad.Trans.State (State, modify, execState)

import Text.LDAP.Data
  (AttrType (..), AttrValue, Attribute,
   Component (..), DN, unconsDN,
   elem', ordW8)
import qualified Text.LDAP.Data as Data


type LdapPutM = State (DList ByteString)
type LdapPrinter a = a -> LdapPutM ()

runLdapPrinter :: LdapPrinter a -> a -> LB.ByteString
runLdapPrinter p = LB.fromChunks . toList . (`execState` mempty) . p

string :: LdapPrinter ByteString
string s = modify (<> pure s)

bslash :: Word8
bslash =  ordW8 '\\'

escapeValueChar :: Word8 -> [Word8]
escapeValueChar w
  | w `elem'` echars                      =  [bslash, w]
  | c /= '\r' && c /= '\n' && isPrint c   =  [w]
  | otherwise                             =  (bslash :) . map ordW8 $ showHex w ""
  where c = chr $ fromIntegral w
        echars = bslash : Data.quotation : Data.specialChars

escapseValueBS :: ByteString -> ByteString
escapseValueBS =  BS.pack . concatMap escapeValueChar . BS.unpack

char :: LdapPrinter Char
char =  string . singleton

-- DN
attrType :: LdapPrinter AttrType
attrType =  d  where
  d (AttrType s)         =  string s
  d (AttrOid (x :| xs))  =  do
    string x
    mapM_ (\x' ->  char '.' >> string x') xs

attrValue :: LdapPrinter AttrValue
attrValue =  string . escapseValueBS

attribute :: LdapPrinter Attribute
attribute (t, v) =  do
  attrType  t
  char '='
  attrValue v

component :: LdapPrinter Component
component =  d  where
  d (S a)          =  attribute a
  d (L (a :| as))  =  do
    attribute a
    mapM_ (\a' -> char '+' >> attribute a') as

dn :: LdapPrinter DN
dn =  d . unconsDN where
  d (c, cs)  =  do
    component c
    mapM_ (\c' -> char ',' >> component c') cs


-- LDIF

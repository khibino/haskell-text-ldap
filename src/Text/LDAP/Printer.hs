{-# LANGUAGE OverloadedStrings #-}

module Text.LDAP.Printer
       ( LdapPrinter, runLdapPrinter, LdapPutM

       , dn
       , component
       , attrType

       , ldifDN, ldifAttr

       , openLdapEntry, openLdapData
       ) where

import Prelude hiding (reverse)
import Data.DList (DList, toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Char (chr, isAscii, isPrint)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (ByteString, singleton)
import qualified Data.ByteString.Lazy as LB
import Control.Applicative (pure)
import Control.Monad.Trans.Writer (Writer, tell, execWriter)
import Text.Printf (printf)

import Text.LDAP.Data
  (AttrType (..), AttrValue, Attribute,
   Component (..), DN, unconsDN,
   LdifAttrValue (..),
   elem', ordW8)
import qualified Text.LDAP.Data as Data


type LdapPutM = Writer (DList ByteString)
type LdapPrinter a = a -> LdapPutM ()

runLdapPrinter :: LdapPrinter a -> a -> LB.ByteString
runLdapPrinter p = LB.fromChunks . toList . execWriter . p

string :: LdapPrinter ByteString
string =  tell . pure

bslash :: Word8
bslash =  ordW8 '\\'

chrW8 :: Word8 -> Char
chrW8 =  chr . fromIntegral

escapeValueChar :: Word8 -> [Word8]
escapeValueChar w
  | not $ isAscii c                       =  hex
  | w `elem'` echars                      =  [bslash, w]
  | c == '\r' || c == '\n'                =  hex
  | isPrint c                             =  [w]
  | otherwise                             =  hex
  where c      = chrW8 w
        echars = bslash : Data.quotation : Data.specialChars
        hex    = (bslash :) . map ordW8 $ printf "%02x" w

_testEscape :: IO ()
_testEscape =
  putStr $ unlines [ show (w, map chrW8 $ escapeValueChar w) | w <- [0 .. 255 ] ]

escapseValueBS :: ByteString -> ByteString
escapseValueBS =  BS.pack . concatMap escapeValueChar . BS.unpack

char :: LdapPrinter Char
char =  string . singleton

newline :: LdapPutM ()
newline =  char '\n'

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
ldifDN :: LdapPrinter DN
ldifDN x = do
  string "dn: "
  dn x

ldifAttrValue :: LdapPrinter LdifAttrValue
ldifAttrValue = d  where
  d (LAttrValRaw s)    = do
    char ' '
    string s
  d (LAttrValBase64 s) = do
    string ": "
    string s

ldifAttr :: LdapPrinter (AttrType, LdifAttrValue)
ldifAttr (a, v) = do
  attrType a
  ldifAttrValue v

openLdapEntry :: LdapPrinter (DN, [(AttrType, LdifAttrValue)])
openLdapEntry (x, as) = do
  ldifDN x
  newline
  mapM_ ((>> newline) . ldifAttr) as

openLdapData :: LdapPrinter [(DN, [(AttrType, LdifAttrValue)])]
openLdapData = mapM_ ((>> newline) . openLdapEntry)

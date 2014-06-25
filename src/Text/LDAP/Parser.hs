{-# LANGUAGE OverloadedStrings #-}

module Text.LDAP.Parser
       ( LdapParser, runLdapParser
       , dn
       , component
       , attribute

       , ldifDN, ldifAttr, decodeLdifAttrValue

       , openLdapEntry, openLdapData, openLdapDataBlocks
       ) where

import Control.Applicative
  ((<$>), pure, (<*>), (*>), (<*), (<|>), some, many)
import Numeric (readHex)
import Data.Monoid ((<>))
import Data.Word (Word8)
import Data.Char (ord)
import Data.ByteString (ByteString, pack)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LB
import Data.Attoparsec.ByteString.Char8
  (Parser, satisfy, isAlpha_ascii, char, char8)
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Attoparsec.ByteString.Lazy (parse, eitherResult)
import qualified Data.ByteString.Base64 as Base64

import Text.LDAP.Data
  (AttrType (..), AttrValue, Attribute, Component, DN, exact, inBounds, notElem',
   LdifAttrValue (..))
import qualified Text.LDAP.Data as Data


type LdapParser = Parser

runLdapParser :: Parser a -> LB.ByteString -> Either String a
runLdapParser p = eitherResult . parse (p <* AP.endOfInput)


word8 :: Char -> Word8
word8 =  fromIntegral . ord

satisfyW8 :: (Char -> Bool) -> LdapParser Word8
satisfyW8 =  (word8 <$>) . satisfy

spaces :: LdapParser ()
spaces =  many (char ' ') *> pure ()

alpha :: LdapParser Char
alpha =  satisfy isAlpha_ascii

alphaW8 :: LdapParser Word8
alphaW8 =  word8 <$> alpha

digit :: LdapParser Char
digit =  AP.digit

digitW8 :: LdapParser Word8
digitW8 =  word8 <$> digit

quotation :: LdapParser Word8
quotation =  char8 Data.quotation

digits1' :: LdapParser ByteString
digits1' =  pack <$> some digitW8


-- DN
keychar :: LdapParser Word8
keychar =  word8 <$> (alpha <|> digit <|> char '-')

quotechar :: LdapParser Word8
quotechar =  satisfyW8 (`notElem'` ['\\', Data.quotation])

special :: LdapParser Word8
special =  satisfyW8 (`elem` Data.specialChars)

stringchar :: LdapParser Word8
stringchar =  satisfyW8 (`notElem'` '\r' : '\n' : '\\' : Data.quotation : Data.specialChars)

hexchar :: LdapParser Char
hexchar =  digit <|> satisfy (`inBounds` [('a', 'f'), ('A', 'F')])


hexpair :: LdapParser Word8
hexpair =  (rh <$>) $ (:) <$> hexchar <*> ((:) <$> hexchar <*> pure [])  where
  rh s
    | rs == []   =  error $ "hexpair: BUG!: fail to read hex: " ++ s
    | otherwise  =  fst $ head rs
    where rs = readHex s

pair :: LdapParser Word8
pair =  char '\\' *> (
  special       <|>
  char8 '\\'    <|>
  quotation     <|>
  hexpair )


hexstring :: LdapParser ByteString
hexstring =  pack <$> some hexpair

string :: LdapParser ByteString
string =  pack <$> some (stringchar <|> pair)  <|>
          char '#' *> hexstring                <|>
          pack <$> (quotation *> many (quotechar <|> pair) <* quotation) {- Only for v2 -} <|>
          pure ""

_testString :: Either String ByteString
_testString =  runLdapParser string "\",\""

attrOid :: LdapParser AttrType
attrOid =  Data.attrOid <$> digits1' <*> many (char '.' *> digits1')

attrTypeStr :: LdapParser AttrType
attrTypeStr =  (Data.AttrType . pack <$>) $ (:) <$> alphaW8 <*> many keychar

attrType :: LdapParser AttrType
attrType =  attrTypeStr <|> attrOid

_testAT :: Either String AttrType
_testAT =  runLdapParser attrType "dc"

attrValue :: LdapParser AttrValue
attrValue =  string

_testAV :: Either String AttrValue
_testAV =  runLdapParser attrValue "com"

attribute :: LdapParser Attribute
attribute =  Data.Attribute
             <$> (attrType <* char '=')
             <*>  attrValue

_testAttr :: Either String Attribute
_testAttr =  runLdapParser attribute "dc=com"

component :: LdapParser Component
component =  Data.component <$> attribute <*> many (char '+' *> attribute)

comma :: LdapParser Char
comma =  spaces *> (char ',' <|> char ';') <* spaces

dn :: LdapParser DN
dn =  Data.textDN <$> component <*> many (comma *> component)



-- LDIF
fill :: LdapParser ()
fill =  spaces

base64Bounds :: [(Char, Char)]
base64Bounds =  [('A', 'Z'), ('a', 'z'), ('0', '9'), exact '+', exact '/', exact '=']

base64String :: LdapParser ByteString
base64String =  pack <$> many (satisfyW8 (`inBounds` base64Bounds))

padDecodeB64 :: ByteString -> Either String ByteString
padDecodeB64 s = Base64.decode (s <> pad)  where
  pad = BS8.replicate ((- BS8.length s) `mod` 4) '='

decodeBase64 :: ByteString -> LdapParser ByteString
decodeBase64 =
  either (fail . ("decodeBase64: " ++)) pure . padDecodeB64

parseDN :: ByteString -> LdapParser DN
parseDN s = do
  either (fail . ("internal parseDN: " ++)) pure
    . runLdapParser dn $ LB.fromChunks [s]

ldifSafeString :: LdapParser ByteString
ldifSafeString =
  (pack <$>)
  $ (:)
  <$> satisfyW8 (`inBounds` Data.ldifSafeInitBounds)
  <*> many (satisfyW8 (`inBounds` Data.ldifSafeBounds))

ldifDN :: LdapParser DN
ldifDN =  AP.string "dn:" *> (
  fill *> dn                                              <|>
  char ':' *> fill *> (parseDN =<< decodeBase64 =<< base64String)
  )

ldifAttr :: LdapParser (AttrType, LdifAttrValue)
ldifAttr =
  (,)
  <$> (attrType <* char ':')
  <*> (
    fill             *> (LAttrValRaw    <$> ldifSafeString)  <|>
    char ':' *> fill *> (LAttrValBase64 <$> base64String)    <|>
    pure (LAttrValRaw "")
    )

newline :: LdapParser ByteString
newline =  AP.string "\n" <|> AP.string "\r\n"

openLdapEntry :: LdapParser (DN, [(AttrType, LdifAttrValue)])
openLdapEntry =
  (,)
  <$> (ldifDN <* newline)
  <*> many (ldifAttr <* newline)

decodeLdifAttrValue :: LdifAttrValue -> Either String AttrValue
decodeLdifAttrValue a = case a of
  LAttrValRaw    s -> Right s
  LAttrValBase64 b -> padDecodeB64 b

openLdapData :: LdapParser [(DN, [(AttrType, LdifAttrValue)])]
openLdapData =  many (openLdapEntry <* newline)

contLines :: [LB.ByteString] -> [LB.ByteString]
contLines =  d  where
  d  []    = []
  d (x:xs) = rec' x xs  where
    rec' a []     = [a]
    rec' a (y:ys)
      | hd == " "  =  rec' (a <> tl) ys
      | otherwise  =  a : rec' y ys
      where (hd, tl) = LB.splitAt 1 y

blocks :: [LB.ByteString] -> [[LB.ByteString]]
blocks =  d  where
  d     []     =  []
  d ls@(_:_)   =  hd : blocks (drop 1 tl)
    where  (hd,tl) = break (== "") ls

openLdapDataBlocks :: [LB.ByteString] -> [[LB.ByteString]]
openLdapDataBlocks =  map contLines . blocks

_test0 :: Either String DN
_test0 =  runLdapParser ldifDN "dn: cn=Slash\\\\The Post\\,ma\\=ster\\+\\<\\>\\#\\;,dc=example.sk,dc=com"

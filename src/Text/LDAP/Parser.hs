module Text.LDAP.Parser
       ( LdapParser, runLdapParser
       , dn
       , component
       , attribute
       ) where

import Control.Applicative
  ((<$>), pure, (<*>), (*>), (<*), (<|>), some, many)
import Numeric (readHex)
import Data.Word (Word8)
import Data.Char (isAscii, ord)
import Data.ByteString (ByteString, pack)
import qualified Data.ByteString.Lazy as LB
import Data.Attoparsec.ByteString.Char8
  (Parser, satisfy, isAlpha_ascii)
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Attoparsec.ByteString.Lazy (parse, eitherResult)

import Text.LDAP.Data (AttrType (..), Attribute, Component, DN)
import qualified Text.LDAP.Data as Data


type LdapParser = Parser

runLdapParser :: Parser r -> LB.ByteString -> Either String r
runLdapParser p = eitherResult . parse p


word8 :: Char -> Word8
word8 =  fromIntegral . ord

char :: Char -> LdapParser Char
char =  AP.char

alpha :: LdapParser Char
alpha =  satisfy isAlpha_ascii

alphaW8 :: LdapParser Word8
alphaW8 =  word8 <$> alpha

digit :: LdapParser Char
digit =  AP.digit

digitW8 :: LdapParser Word8
digitW8 =  word8 <$> digit

quotation :: LdapParser Char
quotation =  char Data.quotation

digits1' :: LdapParser ByteString
digits1' =  pack <$> some digitW8


keychar :: LdapParser Word8
keychar =  word8 <$> (alpha <|> digit <|> char '-')

quotechar :: LdapParser Word8
quotechar =  word8 <$> satisfy isAscii

special :: LdapParser Char
special =  satisfy (`elem` Data.specialChars)

stringchar :: LdapParser Word8
stringchar =  word8 <$> satisfy (`notElem` '\\' : Data.specialChars)

hexchar :: LdapParser Char
hexchar =  digit <|> satisfy (`elem` ['a' .. 'f'] ++ ['A' .. 'F'])


hexpair :: LdapParser Word8
hexpair =  (rh <$>) $ (:) <$> hexchar <*> ((:) <$> hexchar <*> pure [])  where
  rh s
    | rs == []   =  error $ "hexpair: BUG!: fail to read hex: " ++ s
    | otherwise  =  fst $ head rs
    where rs = readHex s

pair :: LdapParser Word8
pair =  char '\\' *> (
  fromIntegral . ord <$> (
     special    <|>
     char '\\'  <|>
     quotation)            <|>
  hexpair)


hexstring :: LdapParser ByteString
hexstring =  pack <$> some hexpair

string :: LdapParser ByteString
string =  pack <$> many (stringchar <|> pair)  <|>
          char '#' *> hexstring                <|>
          pack <$> (quotation *> many (quotechar <|> pair) <* quotation)  --  Only for v2


attrOid :: LdapParser AttrType
attrOid =  Data.attrOid <$> digits1' <*> many (char '.' *> digits1')

attrTypeStr :: LdapParser AttrType
attrTypeStr =  (Data.AttrType . pack <$>) $ (:) <$> alphaW8 <*> some keychar

attrType :: LdapParser AttrType
attrType =  attrTypeStr <|> attrOid

attrValue :: LdapParser ByteString
attrValue =  string

attribute :: LdapParser Attribute
attribute =  Data.Attribute
             <$> (attrType <* char '=')
             <*>  attrValue

component :: LdapParser Component
component =  Data.component <$> attribute <*> some (char '+' *> attribute)

dn :: LdapParser DN
dn =  Data.textDN <$> component <*> some (char ',' *> component)

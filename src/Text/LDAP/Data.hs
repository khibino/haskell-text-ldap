module Text.LDAP.Data
       ( Attribute (..)
       , AttrType (..), attrOid
       , AttrValue

       , Component, component

       , DN, textDN

       , List1

       , quotation, specialChars
       ) where

import Prelude hiding (reverse)
import Data.List.NonEmpty (NonEmpty ((:|)), reverse)
import Data.ByteString (ByteString)


type List1 = NonEmpty

data AttrType
  = AttrType ByteString
  | AttrOid  (List1 ByteString)
  deriving (Eq, Ord, Show)

attrOid :: ByteString -> [ByteString] -> AttrType
attrOid hd tl = AttrOid $ hd :| tl

type AttrValue = ByteString

data Attribute =
  Attribute AttrType AttrValue
  deriving (Eq, Ord, Show)

data Component
  = S Attribute
  | L (List1 Attribute)
  deriving (Eq, Ord, Show)

component :: Attribute -> [Attribute] -> Component
component =  d  where
  d x  []        =  S   x
  d x  xs@(_:_)  =  L $ x :| xs

type DN = List1 Component

textDN :: Component -> [Component] -> DN
textDN h tl = reverse $ h :| tl

quotation :: Char
quotation =  '"'

specialChars :: String
specialChars =  [',', '=', '+', '<', '>', '#', ';']

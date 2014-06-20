module Text.LDAP.Data
       ( Attribute (..)
       , AttrType (..), attrOid
       , AttrValue

       , Component, component

       , DN, textDN

       , List1
       , Bound, boundsElems, inBounds, setElem, inMBounds

       , quotation, specialChars

       , ldifSafeCharBounds,     isLdifSafeChar,     ldifSafeChars
       , ldifSafeInitCharBounds, isLdifSafeInitChar, ldifSafeInitChars
       ) where

import Prelude hiding (reverse)
import Data.List.NonEmpty (NonEmpty ((:|)), reverse)
import Data.ByteString (ByteString)
import Data.Set (fromList, member)


type List1 = NonEmpty

type Bound a = (a, a)

{-# SPECIALIZE boundsElems :: [(Char, Char)] -> [Char] #-}
boundsElems :: Enum a => [(a, a)] -> [a]
boundsElems =  (>>= \(x, y) -> [x .. y])

{-# SPECIALIZE inBounds :: Char -> [(Char, Char)] -> Bool #-}
inBounds :: Ord a => a -> [(a, a)] -> Bool
inBounds a = or . map (\(x, y) -> (x <= a && a <= y))

{-# SPECIALIZE setElem :: Char -> [Char] -> Bool #-}
setElem :: Ord a => a -> [a] -> Bool
setElem a = (a `member`) . fromList

{-# SPECIALIZE inMBounds :: Char -> [(Char, Char)] -> Bool #-}
inMBounds :: (Enum a, Ord a) => a -> [(a, a)] -> Bool
inMBounds a = (a `setElem`) . boundsElems

infix 4 `inBounds`, `setElem`, `inMBounds`

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


-- LDIF
ldifSafeCharBounds :: [(Char, Char)]
ldifSafeCharBounds =
  [ ('\x01', '\x09')
  , ('\x0B', '\x0C')
  , ('\x0E', '\x7F')
  ]

isLdifSafeChar :: Char -> Bool
isLdifSafeChar =  (`inBounds` ldifSafeCharBounds)

ldifSafeChars :: String
ldifSafeChars =  boundsElems ldifSafeCharBounds

ldifSafeInitCharBounds :: [(Char, Char)]
ldifSafeInitCharBounds =
  [ ('\x01', '\x09')
  , ('\x0B', '\x0C')
  , ('\x0E', '\x1F')
  , ('\x21', '\x39')
  , ('\x3B', '\x3B')
  , ('\x3D', '\x7F')
  ]

isLdifSafeInitChar :: Char -> Bool
isLdifSafeInitChar =  (`inBounds` ldifSafeInitCharBounds)

ldifSafeInitChars :: String
ldifSafeInitChars =  boundsElems ldifSafeInitCharBounds

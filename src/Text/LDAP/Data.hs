module Text.LDAP.Data
       ( Attribute (..)
       , AttrType (..), attrOid
       , AttrValue

       , Component, component

       , DN, textDN

       , List1
       , Bound, exact, boundsElems, inBounds, setElem, inMBounds

       , quotation, specialChars

       , ldifSafeBounds,     ldifSafeChars
       , ldifSafeInitBounds, ldifSafeInitChars
       ) where

import Prelude hiding (reverse)
import Data.Ord (comparing)
import Data.List (sortBy)
import Data.ByteString (ByteString)
import Data.Set (fromList, member)
import Data.List.NonEmpty (NonEmpty ((:|)), reverse)


type List1 = NonEmpty

type Bound a = (a, a)

exact :: a -> Bound a
exact a = (a, a)

{-# SPECIALIZE bexpand :: (Char, Char) -> [Char] #-}
bexpand :: Enum a => (a, a) -> [a]
bexpand (x, y) = [x .. y]

{-# SPECIALIZE boundsElems :: [(Char, Char)] -> [Char] #-}
boundsElems :: Enum a => [(a, a)] -> [a]
boundsElems =  (>>= bexpand)

{-# SPECIALIZE widerFirst :: [(Char, Char)] -> [(Char, Char)] #-}
widerFirst :: (Enum a, Ord a) => [(a, a)] -> [(a, a)]
widerFirst =  sortBy (flip $ comparing $ length . bexpand)

{-# SPECIALIZE inBounds :: Char -> [(Char, Char)] -> Bool #-}
inBounds :: (Enum a, Ord a) => a -> [(a, a)] -> Bool
inBounds a = or . map (\(x, y) -> (x <= a && a <= y)) . widerFirst

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
ldifSafeBounds :: [(Char, Char)]
ldifSafeBounds =
  [ ('\x01', '\x09')
  , ('\x0B', '\x0C')
  , ('\x0E', '\x7F')
  ]

ldifSafeChars :: String
ldifSafeChars =  boundsElems ldifSafeBounds

ldifSafeInitBounds :: [(Char, Char)]
ldifSafeInitBounds =
  [ ('\x01', '\x09')
  , ('\x0B', '\x0C')
  , ('\x0E', '\x1F')
  , ('\x21', '\x39')
  , exact '\x3B'
  , ('\x3D', '\x7F')
  ]

ldifSafeInitChars :: String
ldifSafeInitChars =  boundsElems ldifSafeInitBounds

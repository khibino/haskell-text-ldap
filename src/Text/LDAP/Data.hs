module Text.LDAP.Data
       ( Attribute
       , AttrType (..), attrOid
       , AttrValue

       , Component (..), component

       , DN, consDN

       , List1
       , Bound, exact, boundsElems, inBounds, elem', notElem', inMBounds

       , quotation, specialChars, notValueStringChars

       , LdifAttrValue (..)
       , ldifSafeBounds
       , ldifSafeInitBounds
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

{-# SPECIALIZE elem' :: Char -> [Char] -> Bool #-}
elem' :: Ord a => a -> [a] -> Bool
elem' a = (a `member`) . fromList

{-# SPECIALIZE notElem' :: Char -> [Char] -> Bool #-}
notElem' :: Ord a => a -> [a] -> Bool
notElem' a = not . (a `elem'`)

{-# SPECIALIZE inMBounds :: Char -> [(Char, Char)] -> Bool #-}
inMBounds :: (Enum a, Ord a) => a -> [(a, a)] -> Bool
inMBounds a = (a `elem'`) . boundsElems

infix 4 `inBounds`, `elem'`, `notElem'`, `inMBounds`

data AttrType
  = AttrType ByteString
  | AttrOid  (List1 ByteString)
  deriving (Eq, Ord, Show)

attrOid :: ByteString -> [ByteString] -> AttrType
attrOid hd tl = AttrOid $ hd :| tl

type AttrValue = ByteString

type Attribute = (AttrType, AttrValue)

data Component
  = S Attribute
  | L (List1 Attribute)
  deriving (Eq, Ord, Show)

component :: Attribute -> [Attribute] -> Component
component =  d  where
  d x  []        =  S   x
  d x  xs@(_:_)  =  L $ x :| xs

type DN = List1 Component

consDN :: Component -> [Component] -> DN
consDN h tl = reverse $ h :| tl

quotation :: Char
quotation =  '"'

specialChars :: String
specialChars =  [',', '=', '+', '<', '>', '#', ';']

notValueStringChars :: String
notValueStringChars =  '\r' : '\n' : '\\' : quotation : specialChars


-- LDIF
data LdifAttrValue
  = LAttrValRaw    ByteString
  | LAttrValBase64 ByteString
  deriving (Eq, Ord, Show)

ldifSafeBounds :: [(Char, Char)]
ldifSafeBounds =
  [ ('\x01', '\x09')
  , ('\x0B', '\x0C')
  , ('\x0E', '\x7F')
  ]

ldifSafeInitBounds :: [(Char, Char)]
ldifSafeInitBounds =
  [ ('\x01', '\x09')
  , ('\x0B', '\x0C')
  , ('\x0E', '\x1F')
  , ('\x21', '\x39')
  , exact '\x3B'
  , ('\x3D', '\x7F')
  ]

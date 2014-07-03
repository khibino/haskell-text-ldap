-- |
-- Module      : Text.LDAP.InternalParser
-- Copyright   : 2014 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- Module of internal share parsers.
module Text.LDAP.InternalParser
       ( LdapParser
       , satisfyW8

       , ldifSafeString
       ) where

import Control.Applicative ((<$>), (<*>), many)
import Data.Word (Word8)
import Data.ByteString (ByteString, pack)
import Data.Attoparsec.ByteString.Char8 (Parser, satisfy)

import Text.LDAP.Data (Bound, ordW8, inBounds, exact)


type LdapParser = Parser

-- | Char bounds LDIF safe string
ldifSafeBounds :: [Bound Char]
ldifSafeBounds =
  [ ('\x01', '\x09')
  , ('\x0B', '\x0C')
  , ('\x0E', '\x7F')
  ]

-- | Char bounds LDIF safe string first char
ldifSafeInitBounds :: [Bound Char]
ldifSafeInitBounds =
  [ ('\x01', '\x09')
  , ('\x0B', '\x0C')
  , ('\x0E', '\x1F')
  , ('\x21', '\x39')
  , exact '\x3B'
  , ('\x3D', '\x7F')
  ]

satisfyW8 :: (Char -> Bool) -> LdapParser Word8
satisfyW8 =  (ordW8 <$>) . satisfy

ldifSafeString :: LdapParser ByteString
ldifSafeString =
  (pack <$>)
  $ (:)
  <$> satisfyW8 (`inBounds` ldifSafeInitBounds)
  <*> many (satisfyW8 (`inBounds` ldifSafeBounds))

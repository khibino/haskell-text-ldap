{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Text.LDAP.Parser
import qualified Text.LDAP.Parser as Parser
import Text.LDAP.Printer
import qualified Text.LDAP.Printer as Printer
import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Either


appendErr :: String -> Either String a -> Either String a
appendErr s = either (Left . (++ (": " ++ s))) Right

isoTest :: LB.ByteString -> Either String LB.ByteString
isoTest b = do
  let entParser = Parser.openLdapEntry Parser.ldifAttrValue
  ent0 <- appendErr "parse0" $ runLdapParser entParser b
  let out = runLdapPrinter (Printer.openLdapEntry Printer.ldifAttrValue) ent0
  ent1 <- appendErr ("parse1: " ++ LB.unpack out) $ runLdapParser entParser out
  if ent0 == ent1
    then Right $ "Isomorphic: " <> out
    else Left  $ "Not isomorphic: " ++ LB.unpack out

main :: IO ()
main =  do
  bs <- map LB.unlines . openLdapDataBlocks . LB.lines <$> LB.getContents
  mapM_ putStrLn . lefts . map isoTest $ bs


import System.Environment (getArgs)
import Control.Applicative
import Text.LDAP.Data
import Text.LDAP.Parser
import qualified Data.ByteString.Lazy.Char8 as LB


matchTest :: Show a => (LdifAttrValue -> LdapParser a) -> LB.ByteString -> String
matchTest d b = case runLdapParser (openLdapEntry d) b of
  Right r -> show r
  Left  e -> "Failed: " ++ show e ++ ": " ++ show b

main :: IO ()
main =  do
  as <- getArgs
  let test = case as of
        "raw":_  -> matchTest rawAttrValue
        _        -> matchTest decodeAttrValue

  bs <- map LB.unlines . openLdapDataBlocks . LB.lines <$> LB.getContents
  mapM_ (putStrLn . test) $ bs

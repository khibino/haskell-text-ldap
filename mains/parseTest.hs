
import Control.Applicative
import Text.LDAP.Parser
import qualified Data.ByteString.Lazy.Char8 as LB
---  import Data.Either


matchTest :: LB.ByteString -> String
matchTest b = case runLdapParser (openLdapEntry decodeLdifAttrValue) b of
  Right r -> show r
  Left  e -> "Failed: " ++ show e ++ ": " ++ show b

main :: IO ()
main =  do
  bs <- map LB.unlines . openLdapDataBlocks . LB.lines <$> LB.getContents
  mapM_ (putStrLn . matchTest) $ bs

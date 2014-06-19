
import Text.LDAP.Parser
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Either

main :: IO ()
main =  do
  in' <- LB.getContents
  let bs = map LB.unlines . openLdapDataBlocks $ LB.lines in'
  mapM_ print . lefts . map (runLdapParser openLdapEntry) $ bs

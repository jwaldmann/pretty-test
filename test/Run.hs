import Lib
import Data.Proxy
import qualified Text.PrettyPrint.HughesPJ as TPH 
import qualified Text.PrettyPrint.Leijen as TPL
import qualified Text.PrettyPrint.Free as TPF
import System.Environment ( getArgs )

main :: IO ()
main = do
  argv <- getArgs
  case argv of
      [ ] -> check_contexts (Proxy :: Proxy TPH.Doc) 1000
      [ "pretty", s ] -> check_contexts (Proxy :: Proxy TPH.Doc) (read s)
      [ "wl-pprint", s] -> check_contexts (Proxy :: Proxy TPL.Doc) (read s)
      [ "wl-pprint-extras", s] -> check_contexts (Proxy :: Proxy (TPF.Doc ())) (read s)
  return ()

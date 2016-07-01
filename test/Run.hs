import Lib
import Data.Proxy
import System.Timeout
import qualified Text.PrettyPrint.HughesPJ as TPH 
import qualified Text.PrettyPrint.Leijen as TPL
import qualified Text.PrettyPrint.Free as TPF
import System.Environment ( getArgs )
import Control.Monad ( void)

-- | when called with no arguments, runs for 10 seconds
-- (because this happens from 'cabal test').
-- when called with arguments, runs forever.
main :: IO ()
main = do
  argv <- getArgs
  case argv of
      [ ] -> void $ timeout (10 * 10^6) $ check_contexts (Proxy :: Proxy TPH.Doc) 1000
      [ "pretty", s ] -> void $ check_contexts (Proxy :: Proxy TPH.Doc) (read s)
      [ "wl-pprint", s] -> void $ check_contexts (Proxy :: Proxy TPL.Doc) (read s)
      [ "wl-pprint-extras", s] -> void $ check_contexts (Proxy :: Proxy (TPF.Doc ())) (read s)

{-# language DataKinds #-}
{-# language TypeApplications #-}

import Lib
import System.Timeout

import System.Environment ( getArgs )
import Control.Monad ( void)

-- | when called with no arguments, runs for 10 seconds
-- (because this happens from 'cabal test').
-- when called with arguments, runs forever.
main :: IO ()
main = do
  argv <- getArgs
  case argv of
      [ ] -> void $ timeout (10 * 10^6) $ check_contexts (Proxy :: Proxy 'TPH) 1000
      [ "pretty", s ] -> void $ check_contexts (Proxy :: Proxy 'TPH ) (read s)
      [ "wl-pprint", s] -> void $ check_contexts (Proxy :: Proxy 'TPL) (read s)
      [ "wl-pprint-extras", s] -> void $ check_contexts (Proxy :: Proxy 'TPF) (read s)
      -- [ "wl-pprint-text", s] -> void $ check_contexts (Proxy :: Proxy 'TPF) (read s)
      [ "ansi-wl-pprint", s] -> void $ check_contexts (Proxy :: Proxy 'TPAL) (read s)
      [ "mainland-pretty", s] -> void $ check_contexts (Proxy :: Proxy 'TPM) (read s)


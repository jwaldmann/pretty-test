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
  let it = 1000 ; to = 10
  case argv of
      [ ] -> void $ check_contexts (Proxy :: Proxy 'TPH) it to
      [ "pretty", s ] -> void $ check_contexts (Proxy :: Proxy 'TPH ) (read s) to
      [ "wl-pprint", s] -> void $ check_contexts (Proxy :: Proxy 'TPL) (read s) to
      [ "wl-pprint-text", s] -> void $ check_contexts (Proxy :: Proxy 'TPLT) (read s) to
      [ "wl-pprint-extras", s] -> void $ check_contexts (Proxy :: Proxy 'TPF) (read s) to
      [ "ansi-wl-pprint", s] -> void $ check_contexts (Proxy :: Proxy 'TPAL) (read s) to
      [ "mainland-pretty", s] -> void $ check_contexts (Proxy :: Proxy 'TPM) (read s) to
      _ -> error $ show argv

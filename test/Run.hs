{-# language DataKinds #-}

import Lib
import System.Timeout

import System.Environment ( getArgs )
import System.Timeout (timeout)
import Control.Monad ( void, forM_ )

-- | when called with no arguments (this happens from 'cabal test')
-- runs for 20 seconds for each printer.
-- 
-- when called with arguments 
-- (printer, iteration depth, timeout per case), 
-- runs forever.
main :: IO ()
main = do
  argv <- getArgs
  case argv of
      [ ] -> forM_ printers $ \ p -> void $ timeout (20 * 10^6) $ run [ p ]
      _ -> run argv

printers = 
  [ "prettyprinter-pretty", "prettyprinter-smart", "prettyprinter-compact"
  , "pretty"
  , "wl-pprint", "wl-pprint-text", "wl-pprint-extras"
  , "ansi-wl-pprint"
  , "mainland-pretty"
  ]

run (argv @ (p : rest)) = do
  let bar = replicate 80 '*'
  putStr $ unlines [ "", bar, unwords $ argv , bar ]
  let (it,to) = case rest of
         []    -> (500, 10)
         [s]   -> (read s, 10)
         [s,t] -> (read s, read t)
  case p of
      "pretty"           -> void $ check_contexts (Proxy :: Proxy 'TPH ) it to
      "wl-pprint"        -> void $ check_contexts (Proxy :: Proxy 'TPL ) it to
      "wl-pprint-text"   -> void $ check_contexts (Proxy :: Proxy 'TPLT) it to
      "wl-pprint-extras" -> void $ check_contexts (Proxy :: Proxy 'TPF ) it to
      "ansi-wl-pprint"   -> void $ check_contexts (Proxy :: Proxy 'TPAL) it to
      "mainland-pretty"  -> void $ check_contexts (Proxy :: Proxy 'TPM ) it to
      "prettyprinter-pretty"   -> void $ check_contexts (Proxy :: Proxy ('DTP Pretty) ) it to
      "prettyprinter-smart"    -> void $ check_contexts (Proxy :: Proxy ('DTP Smart ) ) it to
      "prettyprinter-compact"  -> void $ check_contexts (Proxy :: Proxy ('DTP Compact) ) it to

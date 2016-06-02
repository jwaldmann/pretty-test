import Lib
import System.Environment ( getArgs )

main :: IO ()
main = do
  argv <- getArgs
  case argv of
      [ ] -> check_contexts  100     1000
      [s] -> check_contexts (read s) 1000
  return ()

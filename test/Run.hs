import Lib
import System.Environment ( getArgs )

main :: IO ()
main = do
  argv <- getArgs
  case argv of
      [ ] -> check_contexts        3 100
      [s] -> check_contexts (read s) 100
  return ()

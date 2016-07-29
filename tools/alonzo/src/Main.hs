
module Main where

import Phase1 ( phase1 )
import Phase2 ( phase2 )
import Phase3 ( phase3 )



main :: IO ()
main =
  return ()


usage :: IO ()
usage = do
    putStrLn $ unlines [
        "alonzo <flag>* [<file>*]",
        "  --help        This usage information."
        "  --version",
    ]

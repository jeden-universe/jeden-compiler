
module Jeden.Test where

import Jeden.Phase1
import Jeden.Phase2

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Text.PrettyPrint.GenericPretty ( Out, pp )

type ParseFun a = String -> Either String a
type Verbosity  = Int

runFile :: (Show a, Out a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: (Show a, Out a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s = case p s of
           Left s   -> do
               putStrLn "\nParse              Failed...\n"
               putStrLn s
               exitFailure
           Right tree -> do
               putStrLn "\nParse Successful!"
               if v > 1
                   then pp tree
                   else return ()
               exitSuccess

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> hGetContents stdin >>= run 2 parse
    "-s":fs -> mapM_ (runFile 0 parse) fs
    fs -> mapM_ (runFile 2 parse) fs
  where parse s = phase1 s >>= phase2

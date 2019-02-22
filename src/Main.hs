module Main where

import System.Process (spawnProcess, waitForProcess, ProcessHandle)
import System.IO (stdout, hFlush)
import Debug.Trace (traceShowId)
import Hash.Type
import Hash.Parser

prompt = putStr "Hash> " >> hFlush stdout

main :: IO ()
main = do
  prompt
  line <- getLine
  case parseLine line of
    Left err -> print err
    Right expr -> print expr --execSingleInstruction instruction
  main
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment (lookupEnv)
import System.IO (stdout, stdin, stderr, hFlush)
import System.Exit (ExitCode(..), exitWith)
import Data.Maybe (fromMaybe)
import Control.Exception (catch, IOException)

import Hash.Utils (forkWait)
import Hash.Parser (parseLine)
import Hash.Evaluator (execExpr)

prompt = do
  p <- fromMaybe "Hash> " <$> lookupEnv "PROMPT"
  putStr p >> hFlush stdout

main :: IO ()
main = do
  prompt
  line <- catch getLine $ \(_ :: IOException) -> exitWith ExitSuccess
  case parseLine line of
    Left err -> print err
    Right expr -> do
      execExpr (stdin, stdout) expr
      hFlush stdout
      return ()
  main
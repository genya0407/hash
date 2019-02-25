{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Environment (lookupEnv)
import System.IO (stdout, stdin, stderr, hFlush, hSetBuffering, hGetChar, BufferMode(..))
import System.Exit (ExitCode(..), exitWith)
import System.Console.Haskeline
import Data.Maybe (fromMaybe)
import Control.Exception (catch, IOException)
import Control.Monad.IO.Class (liftIO)

import Hash.Utils (forkWait)
import Hash.Parser (parseLine)
import Hash.Evaluator (execExpr)

prompt = fromMaybe "Hash> " <$> lookupEnv "PROMPT"
inputSettings = Settings { complete = completeFilename, historyFile = Nothing, autoAddHistory = True }
 
main :: IO ()
main = runInputT inputSettings repl
  where
    repl :: InputT IO ()
    repl = do
      minput <- liftIO prompt >>= getInputLine
      case minput of
        Nothing -> return ()
        Just "" -> repl
        Just line -> do
          liftIO $ do
            case parseLine line of
              Left err -> print err
              Right expr -> do
                originalStdin <- hDuplicate stdin
                originalStdout <- hDuplicate stdout
                execExpr (stdin, stdout) expr
                hFlush stdout
                hDuplicateTo originalStdin stdin
                hDuplicateTo originalStdout stdout
                return ()
          repl
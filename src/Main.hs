module Main where

import GHC.IO.Handle (Handle, hClose, hDuplicateTo)
import System.Process (createPipe)
import System.Exit (ExitCode(..))
import System.Posix.Process (forkProcess, executeFile, getProcessStatus, ProcessStatus(..))
import System.Posix.Types (ProcessID)
import System.IO (stdout, stdin, hFlush)
import Debug.Trace (traceShowId)
import Data.Maybe (fromJust)
import Hash.Type
import Hash.Parser

prompt = putStr "Hash> " >> hFlush stdout

type Status = Integer
type InputHandle = Handle
type OutputHandle = Handle

waitPid :: ProcessID -> IO ExitCode
waitPid pid = do
  status <- getProcessStatus True False pid
  case status of
    Just (Exited exitcode) -> return exitcode
    _ -> undefined -- FIXME

hDuplicateTo' h1 h2 = if h1 == h2 then return () else hDuplicateTo h1 h2

execExpr :: (InputHandle, OutputHandle) -> Expression -> IO ExitCode
execExpr (input, output) (Single cmd args) = do
  pid <- forkProcess $ do
    hDuplicateTo' input stdin
    hDuplicateTo' output stdout
    let searchPath = not ('/' `elem` cmd)
    executeFile cmd searchPath args Nothing
  waitPid pid

main :: IO ()
main = do
  prompt
  line <- getLine
  case parseLine line of
    Left err -> print err
    Right expr -> execExpr (stdin, stdout) expr >> return () --execSingleInstruction instruction
  main
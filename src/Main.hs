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
  print status
  case status of
    Just (Exited exitcode) -> return exitcode
    _ -> undefined -- FIXME

hDuplicateTo' h1 h2 = if h1 == h2 then return () else hDuplicateTo h1 h2

spawnExpr :: (InputHandle, OutputHandle) -> Expression -> IO ProcessID
spawnExpr (input, output) (Piped expr1 expr2) = do
  forkProcess $ do
    (outputPipe, inputPipe) <- createPipe
    spawnExpr (input, inputPipe) expr1
    execExpr (outputPipe, output) expr2
    return ()
spawnExpr (input, output) (Single cmd args) = do
  forkProcess $ do
    hDuplicateTo' input stdin
    hDuplicateTo' output stdout
    let searchPath = not ('/' `elem` cmd)
    executeFile cmd searchPath args Nothing

execExpr :: (InputHandle, OutputHandle) -> Expression -> IO ExitCode
execExpr handle expr = spawnExpr handle expr >>= waitPid

main :: IO ()
main = do
  prompt
  line <- getLine
  case parseLine line of
    Left err -> print err
    Right expr -> execExpr (stdin, stdout) expr >> return () --execSingleInstruction instruction
  main
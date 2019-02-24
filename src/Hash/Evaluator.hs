{-# LANGUAGE ScopedTypeVariables #-}
module Hash.Evaluator(execExpr) where

import GHC.IO.Handle (Handle, hClose)
import System.Process (createPipe)
import System.Exit (ExitCode(..), exitWith)
import System.Posix.Process (forkProcess, executeFile, getProcessStatus, ProcessStatus(..))
import System.Posix.Types (ProcessID)
import System.IO (stdout, stdin, stderr, hFlush, openFile, IOMode(ReadMode, WriteMode))
import Hash.Type (Expression(..))
import Hash.Utils (hDuplicateTo', waitPid, forkWait)

type InputHandle = Handle
type OutputHandle = Handle

execExpr :: (InputHandle, OutputHandle) -> Expression -> IO ExitCode
execExpr handles (Block expr1 expr2) = do
  forkWait $ execExpr handles expr1 >> return ()
  forkWait $ execExpr handles expr2 >> return ()
execExpr handles (And expr1 expr2) = do
  status <- forkWait $ execExpr handles expr1 >> return ()
  if status == ExitSuccess
  then
    execExpr handles expr2
  else
    return status
execExpr handles (Or expr1 expr2) = do
  status <- forkWait $ execExpr handles expr1 >> return ()
  if status == ExitSuccess
  then
    return status
  else
    execExpr handles expr2
execExpr (input, output) (Piped expr1 expr2) = do
  (readPipe, writePipe) <- createPipe
  forkProcess $ do
    hClose readPipe
    execExpr (input, writePipe) expr1
    return ()
  hClose writePipe
  execExpr (readPipe, output) expr2
execExpr (input, output) (Single cmd args fnameStdin fnameStdout fnameStderr) = do
  case fnameStdin of
    Just fname -> openFile fname ReadMode >>= flip hDuplicateTo' stdin
    Nothing -> hDuplicateTo' input stdin
  case fnameStdout of
    Just fname -> openFile fname WriteMode >>= flip hDuplicateTo' stdout
    Nothing -> hDuplicateTo' output stdout
  case fnameStderr of
    Just fname -> openFile fname WriteMode >>= flip hDuplicateTo' stderr
    Nothing -> return ()

  let searchPath = not ('/' `elem` cmd)
  executeFile cmd searchPath args Nothing

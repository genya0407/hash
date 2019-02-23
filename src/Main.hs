module Main where

import GHC.IO.Handle (Handle, hClose, hDuplicateTo)
import System.Process (createPipe)
import System.Posix.Process (forkProcess, executeFile, getProcessStatus)
import System.Posix.Types (ProcessID)
import System.IO (stdout, hFlush)
import Debug.Trace (traceShowId)
import Data.Maybe (fromJust)
import Hash.Type
import Hash.Parser

prompt = putStr "Hash> " >> hFlush stdout

type Status = Integer
type InputHandle = Handle
type OutputHandle = Handle

waitPid = fromJust <$> getProcessStatus True False

execExpr :: (InputHandle, OutputHandle) -> Expression -> IO Status
execExpr handles (Block expr1 expr2) = execExpr handles expr1 >> execExpr handles expr2
execExpr handles (And expr1 expr2) = do
  status <- execExpr handles expr1
  if status == 0
  then
    execExpr handles expr2
  else
    return status
execExpr handles (Or expr1 expr2) = do
  status <- execExpr handles expr1
  if status != 0
  then
    execExpr handles expr2
  else
    return status
execExpr (input, output) (Piped expr1 expr2) = do
  status <- waitPid =<< spawnExpr (input, output)
  hClose input
  hClose output
  return status
execExpr (input, output) (Single cmd args) = do
  pid <- spawnExpr (input, output) (Single cmd args)
  waitPid pid

spawnExpr (inputOutside, outputOutside) (Piped expr1 expr2) = do
  (input, output) <- createPipe
  _   <- spawnExpr (inputOutside, output) expr1
  pid <- spawnExpr (input, outputOutside) expr2
  return pid

spawnExpr (input, output) (Single cmd args) = do
  pid <- forkProcess $ do
    hDuplicateTo input stdin
    hDuplicateTo output stdout
    let searchPath = not ('/' `elem` cmd)
    executeFile cmd searchPath args Nothing
  return pid

main :: IO ()
main = do
  prompt
  line <- getLine
  case parseLine line of
    Left err -> print err
    Right expr -> print expr --execSingleInstruction instruction
  main
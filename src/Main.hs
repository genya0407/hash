module Main where

import Text.Parsec
import Text.Parsec.Char
import System.Process (spawnProcess, waitForProcess, ProcessHandle)
import System.IO (stdout, hFlush)

data Instruction = Instruction { instructionCmd :: String,  instructionArgs :: [String] }
execInstruction :: Instruction -> IO ProcessHandle
execInstruction Instruction { instructionCmd = cmd, instructionArgs = args } = spawnProcess cmd args

shellCmd = do
  try spaces
  cmd:args <- sepBy1 (many1 $ noneOf " ") spaces
  return $ Instruction { instructionCmd = cmd, instructionArgs = args }

prompt = putStr "> " >> hFlush stdout

main :: IO ()
main = do
  prompt
  line <- getLine
  case parse shellCmd "" line of
    Left _ -> putStrLn $ "Invalid input: " ++ line
    Right instruction -> do
      processHandle <- execInstruction instruction
      waitForProcess processHandle
      return ()
  main
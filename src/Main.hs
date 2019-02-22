{-# LANGUAGE FlexibleContexts #-}
module Main where

import Text.Parsec
import Text.Parsec.Char
import System.Process (spawnProcess, waitForProcess, ProcessHandle)
import System.IO (stdout, hFlush)
import Debug.Trace (traceShowId)

-- data SingleInstruction = SingleInstruction { instructionCmd :: String,  instructionArgs :: [String],  }
-- execSingleInstruction :: SingleInstruction -> IO ()
-- execSingleInstruction SingleInstruction { instructionCmd = cmd, instructionArgs = args,  } = do
--   pHandle <- spawnProcess cmd args
--   processHandle <- execSingleInstruction instruction
--   return processHandle

-- data PipedInstruction = PipedInstruction { sourceInstruction :: SingleInstruction, sinkInstruction :: PipedInstruction }
-- execPipedInstruction PipedInstruction {sourceInstruction = source, sinkInstruction = sink} = do

data Expression =
  Block Expression Expression | -- expressions separated by ';'
  And Expression Expression | -- expr1 && expr2
  Or Expression Expression | -- expr1 || expr2
  Piped Expression Expression | -- expr1 | expr2
  Single String [String] -- cmd arg1 arg2
  deriving Show

spaces1 = skipMany1 space
blockExpr = singleExpr `chainl1` exprPipe `chainl1` exprAnd `chainl1` exprOr `chainl1` exprSemi
exprPipe = try $ string "|" >> spaces1 >> return Piped
exprAnd = try $ string "&&" >> spaces1 >> return And
exprOr = try $ string "||" >> spaces1 >> return Or
exprSemi = try $ string ";" >> spaces >> return Block

exprTokens = (try $ between (char '"') (char '"') (many anyChar)) <|> (many1 $ noneOf " ;")

func result = do
  pMaybe <- optionMaybe $ do
    try spaces
    lookAhead (try $ exprTokens)

  case pMaybe of
    Just p -> 
      if p `elem` ["|", "&&", "||", ";"]
      then
        return $ reverse result
      else
        exprTokens >> func (p:result)
    Nothing -> return $ reverse result

singleExpr = do
  cmd:args <- func []
  return $ Single cmd args

prompt = putStr "Hash> " >> hFlush stdout

main :: IO ()
main = do
  prompt
  line <- getLine
  case parse blockExpr "" line of
    Left err -> print err
    Right expr -> print expr --execSingleInstruction instruction
  main
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hash.Parser(parseLine) where

import Text.Parsec
import Text.Parsec.Char
import Safe (headMay, lastMay)
import Hash.Type
import Debug.Trace (traceShowId)

parseLine :: String -> Either ParseError Expression
parseLine = parse blockExpr ""

blockExpr = singleExpr `chainl1` exprPipe `chainl1` exprAnd `chainl1` exprOr `chainl1` exprSemi
exprPipe = try $ string "|" >> (lookAhead . try $ noneOf "|") >> return Piped
exprAnd = try $ string "&&" >> spaces >> return And
exprOr = try $ string "||" >> spaces >> return Or
exprSemi = try $ string ";" >> spaces >> return Block

--exprTokens = (try $ between (char '"') (char '"') (many anyChar)) <|> (many1 $ noneOf " ;")

data CmdToken = Stdin String | Stdout String | Stderr String | Other String deriving Show

singleExpr = do
  spaces
  cmdTokens :: [CmdToken] <- many1 (tryToken exprStdin <|> tryToken exprStdout <|> tryToken exprStderr <|> tryToken exprOther)
  let fnameStdin = lastMay [fname | Stdin fname <- cmdTokens ]
  let fnameStdout = lastMay [fname | Stdout fname <- cmdTokens ]
  let fnameStderr = lastMay [fname | Stderr fname <- cmdTokens ]
  let others = [cmd | Other cmd <- cmdTokens ]
  let cmd = head others
  let args = tail others
  skipMany . try $ (spaces >> string ";" >> spaces >> eof)
  return $ Single cmd args fnameStdin fnameStdout fnameStderr
  where
    exprStdin = exprRedirect "<" Stdin
    exprStdout = exprRedirect ">" Stdout
    exprStderr = exprRedirect "2>" Stderr
    exprRedirect tk constructor = do
      string tk
      try spaces
      fname <- exprOtherString
      return $ constructor fname
    exprOtherString = many1 $ noneOf " |&;><"
    exprOther = Other <$> exprOtherString
    tryToken p = do
      res <- try p
      spaces
      return res

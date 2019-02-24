{-# LANGUAGE FlexibleContexts #-}
module Hash.Parser(parseLine) where

import Text.Parsec
import Text.Parsec.Char
import Hash.Type

parseLine :: String -> Either ParseError Expression
parseLine = parse blockExpr ""

spaces1 = skipMany1 space
blockExpr = singleExpr `chainl1` exprPipe `chainl1` exprAnd `chainl1` exprOr `chainl1` exprSemi
exprPipe = try $ string "|" >> spaces1 >> return Piped
exprAnd = try $ string "&&" >> spaces1 >> return And
exprOr = try $ string "||" >> spaces1 >> return Or
exprSemi = try $ string ";" >> spaces >> return Block

--exprTokens = (try $ between (char '"') (char '"') (many anyChar)) <|> (many1 $ noneOf " ;")

data CmdToken = Stdin String | Stdout String | Stderr String | Other String

singleExpr = do
  try spaces
  cmdTokens <- many1 $ (try exprStdin <|> try exprStdout <|> try exprStderr <|> exprOther)
  let fnameStdins = [fname | Stdin fname <- cmdTokens ]
  let fnameStdouts = [fname | Stdout fname <- cmdTokens ]
  let fnameStderrs = [fname | Stderr fname <- cmdTokens ]
  let others = [cmd | Other cmd <- cmdTokens ]
  let cmd = head others
  let args = tail others
  return $ Single cmd args fnameStdins fnameStdouts fnameStderrs
  -- try $ (string ";" >> eof)
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

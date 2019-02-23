{-# LANGUAGE FlexibleContexts #-}
module Hash.Parser(parseLine) where

import Text.Parsec
import Text.Parsec.Char
import Hash.Type

parseLine :: String -> Either ParseError Expression
parseLine = parse blockExpr ""

spaces1 = skipMany1 space
-- blockExpr = singleExpr `chainl1` exprPipe `chainl1` exprAnd `chainl1` exprOr `chainl1` exprSemi
blockExpr = singleExpr `chainl1` exprPipe --`chainl1` exprAnd `chainl1` exprOr `chainl1` exprSemi
exprPipe = try $ string "|" >> spaces1 >> return Piped
-- exprAnd = try $ string "&&" >> spaces1 >> return And
-- exprOr = try $ string "||" >> spaces1 >> return Or
-- exprSemi = try $ string ";" >> spaces >> return Block

exprTokens = (try $ between (char '"') (char '"') (many anyChar)) <|> (many1 $ noneOf " ;")

parseSingleCommand result = do
  pMaybe <- optionMaybe $ do
    try spaces
    lookAhead (try $ exprTokens)

  case pMaybe of
    Just p -> 
      if p `elem` ["|", "&&", "||", ";"]
      then
        return $ reverse result
      else
        exprTokens >> parseSingleCommand (p:result)
    Nothing -> return $ reverse result

singleExpr = do
  cmd:args <- parseSingleCommand []
  return $ Single cmd args

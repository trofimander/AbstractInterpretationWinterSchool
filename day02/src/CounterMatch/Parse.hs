module CounterMatch.Parse where

import Text.ParserCombinators.Parsec
import CounterMatch.Model
import Data.Char
import Data.List


file = endBy instruction eol

instruction :: GenParser Char st Instruction
instruction =
           inc <|> dec <|> zero <|> (string "stop" >> return STOP)

inc =
   do string "inc "
      var <- var
      return (INC var)

dec =
   do string "dec "
      var <- var
      return (DEC var)

positiveNatural = do
    digits <- many1 baseDigit
    let n = foldl (\x d -> base*x + (digitToInt d)) 0 digits
    seq n (return n)
  where
    base = 10
    baseDigit = digit

zero =
   do string "zero "
      var <- var
      char ' '
      p' <- positiveNatural
      string " else "
      p'' <- positiveNatural
      return (ZERO var p' p'')


var = (char 'x' >> return X) <|> (char 'y' >> return Y) <|> (char 'z' >> return Z)

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parse3cm :: String -> String -> Either ParseError Program
parse3cm filename input = parse file filename input
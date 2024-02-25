module Main where

import Text.Parsec (ParseError, (<?>))
import Text.Parsec.String (Parser)
import Text.Parsec.Token (symbol, lexeme, whiteSpace, commaSep)
import Text.Parsec.Expr (Assoc, Assoc (AssocRight, AssocLeft), Operator(Infix), buildExpressionParser)
import Text.ParserCombinators.Parsec (parse, try, many1, choice, chainl1)
import Text.ParserCombinators.Parsec.Char (anyChar)
import Text.Parsec.Char (oneOf, char, satisfy, string, digit)
import Text.Parsec.Combinator (eof, manyTill, anyToken, count)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit, digitToInt)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

num :: Parser Integer
num = do
    n <- many1 digit
    return (read n)

var :: Parser String
var = do
    fc <- firstChar
    rest <- many nonFirstChar
    return (fc:rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

newtype Parentheses = Parentheses Integer
                   deriving (Eq,Show)

parens :: Parser Parentheses
parens = do
    void $ char '('
    e <- num
    void $ char ')'
    return (Parentheses e)

data SingleAdd = SingleAdd Integer Integer
                 deriving (Eq,Show)

add :: Parser SingleAdd
add = do
    e0 <- many1 digit
    void $ char '+'
    e1 <- many1 digit
    return (SingleAdd (read e0) (read e1))

-------------------------------------------

table = [[op "*" (*) AssocLeft, op "/" div AssocLeft]
        ,[op "+" (+) AssocLeft, op "-" (-) AssocLeft]] where
    op s f = Infix (do {
        string s; return f
    })

number :: Parser Integer
number = do
{ 
    ds <- many1 digit
    ; return (read ds)
} <?> "number"

factor = do
    {
        char '('
        ; x <- expr
        ; char ')'
        ; return x
    }
    <|> number
    <?> "simple expression"

expr :: Parser Integer
expr = buildExpressionParser table factor

-------------------------------------------

price :: Parser Int
price = lexeme (do
    { ds1 <- many1 digit
    ; char '.'
    ; ds2 <- count 2 digit
    ; return (convert 0 (ds1 ++ ds2))
}) <?> "price" where
    convert n [] = n
    convert n (d:ds) = convert (10*n + digitToInt d) ds

receipt :: Parser Bool
receipt = do
    { ps <- many produkt
    ; p <- total
    ; return (sum ps == p)
}

total = do
    { p <- price
    ; symbol "total"
    ; return p
}


produkt = do{ try (symbol "return")
    ; p <- price
    ; semi
    ; return (-p)
} <|> do{ identifier
    ; p <- price
    ; semi
    ; return p
} <?> "produkt"

-------------------------------------------

main :: IO ()
main = putStrLn "Hello, Haskell!"

module Task6 where

{-
В этом файле приведён код, написанный (совместными усилиями) на лекции

Модифицируйте представленный тут парсер таким образом, чтобы он поддерживал унарные операции 
(отрицание и факториал), а также числа с плавающей точкой
-}

import Text.Parsec hiding (digit)

type Parser a = Parsec String () a

-- Numbers
digit :: Parser Char
digit = oneOf ['0'..'9']

digits :: Parser String
digits = many1 digit

fractional :: Parser String
fractional = do
    char '.'
    frac <- digits
    return $ '.' : frac

number :: Parser Double
number = do
    int <- digits
    frac <- option "" fractional
    return $ read $ int ++ frac

-- Unary operators
neg :: Parser Double
neg = do
    spaces
    char '-'
    num <- atom
    spaces
    return $ -num

fact :: Parser Double
fact = do
    spaces
    int <- digits
    char '!'
    spaces
    return $ product [1..(read int)]

-- Binary operators
applyMany :: a -> [a -> a] -> a
applyMany x [] = x
applyMany x (h:t) = applyMany (h x) t

div_ :: Parser (Double -> Double -> Double)
div_ = do
    char '/'
    return (/)

star :: Parser (Double -> Double -> Double)
star = do
    char '*'
    return (*)

plus :: Parser (Double -> Double -> Double)
plus = do
    char '+'
    return (+)

minus :: Parser (Double -> Double -> Double)
minus = do
    char '-'
    return (-)

multiplication :: Parser Double
multiplication = do
    spaces
    lhv <- atom
    spaces
    t <- many tail
    return $ applyMany lhv t
    where tail = 
            do
                f <- star <|> div_
                spaces
                rhv <- atom
                spaces
                return (`f` rhv)

addition :: Parser Double
addition = do
    spaces
    lhv <- multiplication
    spaces
    t <- many tail
    return $ applyMany lhv t
    where tail = 
            do
                f <- plus <|> minus
                spaces
                rhv <- multiplication
                spaces
                return (`f` rhv)

-- Parenthesis
parenthesis :: Parser Double
parenthesis = do
    char '('
    res <- addition
    char ')'
    return res

atom :: Parser Double
atom = try fact <|> try neg <|> number <|> parenthesis

eval :: String -> Either ParseError Double
eval = parse addition ""
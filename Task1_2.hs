module Main where

import Prelude hiding (cos, sin, gcd)

{-
  Задание 1.2
  Необходимо реализовать четыре любые функции в данном файле
-}

factorial :: Double -> Double
factorial 0 = 1
factorial n | n > 0 = n * factorial (n-1)
            | n < 0 = error "arg must be >=0"

-- синус числа (формула Тейлора)
sin :: Double -> Int -> Double

sin x 0 = x
sin x n = sin x (n-1) + (-1)^n*x^(2*n+1) / (factorial (2*n+1))
            where factorial f = fromIntegral $ product [1..f]

-- косинус числа (формула Тейлора)
cos :: Double -> Int -> Double

cos x 0 = 1
cos x n = cos x (n-1) + ((-1)^(n))*x^(2*n) / (factorial (2*n))
            where factorial f = fromIntegral $ product [1..f]


-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer

gcd x y
      | y == 0    = x
      | x == 0    = y
      | otherwise = gcd y (mod x y)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x | (length [n | n <- [2 .. x-1], mod x n == 0]) > 0 = False
          | otherwise = True

main :: IO ()
main = do
       let a = sin 3 10
           b = cos 3 10
           c = gcd 115 10
           d = isPrime 30
       putStrLn "-----------"
       print a
       putStrLn "-----------"
       print b
       putStrLn "-----------"
       print c
       putStrLn "-----------"
       print d
       putStrLn "-----------"

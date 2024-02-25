module FibonacciReverse where

fibonacci :: Integer -> Integer
fibonacci' :: Integer -> Integer -> Integer -> Integer

fibonacci' 0 x _ = x
fibonacci' n x y = fibonacci' (n - 1) y (x + y)

fibonacci n | n >= 0 = fibonacci' n 0 1
            | n < 0 && even n = - (fibonacci $ - n)
            | n < 0 = fibonacci $ - n

module Fibbonacci where

{-
Реализация функции получения n-го числа Фиббоначчи
-}

fibonacci n = helper (0, 1) n where
    helper (r2, r1) n | n == 0 = r2
                      | n == 1 = r1
                      | n > 1  = helper (r1, r2 + r1) (n - 1) 
                      | n < 0  = helper (r1 - r2, r2) (n + 1) 

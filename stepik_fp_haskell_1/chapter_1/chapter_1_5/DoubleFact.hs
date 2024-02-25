module DoubleFact where

doubleFact :: Integer -> Integer
doubleFact' :: Integer -> Integer -> Integer

doubleFact' 0 x = x
doubleFact' 1 x = x
doubleFact' n x = doubleFact' (n - 2) (n * x)

doubleFact 1 = 1
doubleFact 2 = 2
doubleFact n = doubleFact' n 1

main :: IO ()
main = return ()

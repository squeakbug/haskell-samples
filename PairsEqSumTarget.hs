module PairsEqSumTarget where

filterPairs :: Int -> [(Int, Int)] -> [(Int, Int)]
filterPairs target numbers = do 
    (i1, x) <- numbers
    (i2, y) <- numbers
    [(i1, i2) | i1 < i2 && x + y == target]

main :: IO ()
main = do
    putStr "Введите target = "
    targetStr <- getLine
    let target :: Int = read targetStr
    putStr "Введите список чисел = "
    lstStr <- getLine
    let numbers :: [Int] = (map read . words) lstStr
    let enumNumbers = zip [0 .. ] numbers
    let filteredNums = filterPairs target enumNumbers
    let filteredNumsStr = show filteredNums
    putStrLn ("Результат: " ++ filteredNumsStr)

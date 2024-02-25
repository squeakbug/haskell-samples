module Change where

coins :: (Ord a, Num a) => [a]
coins = [2, 3, 7]

change :: (Ord a, Num a) => a -> [[a]]
change 0 = [[]] 
change n
    | n < head coins = [] 
    | otherwise = [cs | c <- coins, c <= n, cs <- partialSolution n c]
    where
        partialSolution :: (Ord a, Num a) => a -> a -> [[a]]
        partialSolution currSum currCoin
            | currSum == currCoin = [[currCoin]]
            | currSum < currCoin = []
            | otherwise = [
                currCoin : rest |
                c <- coins
                , c <= restSum
                , rest <- partialSolution restSum c
                , sum rest <= restSum
            ] where 
                restSum = currSum - currCoin
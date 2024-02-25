qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort xs = let 
    in qsort (filter (\x -> x < head xs) xs) ++ filter (\x -> x == head xs) xs ++ qsort (filter (\x -> x > head xs) xs)
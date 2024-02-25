import Data.List (transpose)
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 xs ys zs = let
    sum3' [] = []
    sum3' (x' : xs') = sum x' : sum3' xs'
    in sum3' $ transpose [xs, ys, zs]
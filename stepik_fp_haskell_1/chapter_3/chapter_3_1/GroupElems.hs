groupElems :: Eq a => [a] -> [[a]]
groupElems xs = let
    groupElems' [] [] [] = []
    groupElems' [] acc last = last : acc
    groupElems' (x' : xs') acc' ys'
        | ys' == [] = groupElems' xs' acc' (x' : ys')
        | head ys' == x' = groupElems' xs' acc' (x' : ys')
        | otherwise = groupElems' xs' (ys' : acc') [x']
    in reverse (groupElems' xs [] [])
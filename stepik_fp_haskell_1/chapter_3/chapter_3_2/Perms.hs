perms :: [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms lst = let
    perms' _ [] = []
    perms' left (x' : xs') = map (x':) (perms (left ++ xs')) ++ (perms' (left ++ [x']) xs')
    in perms' [] lst
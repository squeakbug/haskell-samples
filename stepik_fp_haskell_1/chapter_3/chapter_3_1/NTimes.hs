nTimes:: a -> Int -> [a]
nTimes x y = let 
    nTimes' _ 0 acc' = acc'
    nTimes' x' y' acc' = nTimes' x' (y' - 1) (x' : acc')
    in nTimes' x y []
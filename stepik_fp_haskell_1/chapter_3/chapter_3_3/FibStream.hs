fibStream :: [Integer]
fibStream = let
    fibonacci' x y = x : fibonacci' y (x + y)
    in fibonacci' 0 1

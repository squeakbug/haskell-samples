module Integration where

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = let
    n = 1000000
    fsum f _ _ acc 0 = acc
    fsum f x delta acc n = fsum f (x + delta) delta (acc + f x) (n - 1)
    delta = (b - a) / n
    integration' f a b = delta * ((f a + f b) / 2 + (fsum f (a + delta) delta 0 (n - 1)))
    in if b > a then integration' f a b else - (integration' f b a)

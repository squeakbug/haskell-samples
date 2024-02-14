module TypeConversion where

{-

-}

data Coord a = Coord a a

getCenter :: Double -> Coord Int -> Coord Double
getCenter a (Coord x y) = Coord (a * x' + a / 2) (a * y' + a / 2)
  where
    x' = fromIntegral x
    y' = fromIntegral y

getCell :: Double -> Coord Double -> Coord Int
getCell a (Coord x y) = Coord (round ((x - a / 2) / a)) (round ((y - a / 2) / a))

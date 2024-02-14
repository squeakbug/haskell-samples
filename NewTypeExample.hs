class Shape a where
    getArea :: a -> Float

-- Здесь допустимо использовать newtype, так как
-- используется только один конструктор данных
newtype Square = Square { sideLength :: Float }
instance Shape Square where
    getArea s = sideLength s * sideLength s

newtype Circle = Circle { radius :: Float }
instance Shape Circle where
    getArea c = 2 * pi * radius c

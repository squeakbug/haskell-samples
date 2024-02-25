data Bit = Zero | One deriving (Show, Eq)
data Sign = Minus | Plus deriving (Show, Eq)
data Z = Z Sign [Bit] deriving (Show, Eq)

encodeUnsigned :: Int -> [Bit]
encodeUnsigned 0 = []
encodeUnsigned n = loop n where
    loop 0 = []
    loop x = bitFromInt (mod x 2) : loop (div x 2) where
        bitFromInt b = if b > 0 then One else Zero

decodeUnsigned :: [Bit] -> Int
decodeUnsigned bits = sum bitsValues where
    bitsValues = zipWith pow bits positions
    positions = [0 ..]
    pow Zero idx    = 0
    pow One  idx    = 2 ^ idx

encode :: Int -> Z
encode x = Z sign bits where
    sign = if x < 0 then Minus else Plus
    bits = encodeUnsigned . abs $ x

decode :: Z -> Int
decode (Z _ []) = 0
decode (Z Minus bits) = (-1) * decodeUnsigned bits
decode (Z Plus bits)  = decodeUnsigned bits

doOp :: (Int -> Int -> Int) -> Z -> Z -> Z
doOp op a b = encode (decode a `op` decode b)

add :: Z -> Z -> Z
add = doOp (+)

mul :: Z -> Z -> Z
mul = doOp (*)

seqA :: Integer -> Integer

{-
Реализуйте функцию seqA, находящую элементы следующей рекуррентной последовательности:
a0 = 1
a1 = 2
a2 = 3
a{n+3} = a_{n+2} + a_{n+1} - 2a{n} 
-}

seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA n = let
        seqA' 0 _ _ a = a
        seqA' n a0 a1 a2 = seqA' (n - 1) a1 a2 (a2 + a1 - 2 * a0)
    in seqA' (n - 2) 1 2 3

module SeqNCount where

sum'n'count :: Integer -> (Integer, Integer)

sum'n'count 0 = (0, 1)
sum'n'count num = let
    sum'n'count acc cnt 0 = (acc, cnt)
    sum'n'count acc cnt num = sum'n'count (acc + mod num 10) (cnt + 1) (div num 10)
    in sum'n'count 0 0 (abs num)

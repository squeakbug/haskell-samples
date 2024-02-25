module CheckOdd where

newtype Odd = Odd Integer deriving (Eq, Show)

{-
Reference:
https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Enum.html

For any type that is an instance of class Bounded as well as Enum, the following should hold:

* The calls `succ maxBound` and `pred minBound` should result in a runtime error.
* `fromEnum` and `toEnum` should give a runtime error if the result value is not representable in the result type. For example, toEnum 7 :: Bool is an error.
* `enumFrom` and `enumFromThen` should be defined with an implicit bound, thus:

Minimal complete definition
toEnum, fromEnum
-}

instance Enum Odd where
  toEnum x
    | odd x = Odd $ toInteger x
    | otherwise = error ("X нечетное число: " ++ show x)

  fromEnum (Odd x) = fromInteger x

  succ (Odd x) = Odd $ x + 2

  pred (Odd x) = Odd $ x - 2

  enumFrom x = x : xs where xs = enumFrom $ succ x

  enumFromTo (Odd start) (Odd stop) = map Odd [start, start + 2 .. stop]

  enumFromThen (Odd start) (Odd second) = map Odd [start, second ..]

  enumFromThenTo x0@(Odd start) x1@(Odd second) (Odd stop) = map Odd [start, second .. stop] 

test0 = succ (Odd 1) == Odd 3
test1 = pred (Odd 3) == Odd 1
-- enumFrom
test2 = (take 3 $ [Odd 1 ..]) == [Odd 1,Odd 3,Odd 5]
-- enumFromTo
-- -- По возрастанию
test3 = (take 3 $ [Odd 1..Odd 7]) == [Odd 1,Odd 3,Odd 5]
-- -- По убыванию
test4 = null (take 3 $ [Odd 7..Odd 1])
-- enumFromThen
-- -- По возрастанию
test5 = take 3 [Odd 1, Odd 3 ..] == [Odd 1,Odd 3,Odd 5]
-- -- По убыванию
test6 = take 3 [Odd 3, Odd 1 ..] == [Odd 3,Odd 1,Odd (-1)]
-- enumFromThenTo
-- -- По возрастанию
test7 = [Odd 1, Odd 5 .. Odd 7] == [Odd 1,Odd 5]
-- -- По убыванию
test8 = [Odd 7, Odd 5 .. Odd 1] == [Odd 7,Odd 5,Odd 3,Odd 1]
-- -- x1 < x3 && x1 > x2
test9 = null [Odd 7, Odd 5 .. Odd 11]
-- -- x1 > x3 && x1 < x2
test10 = null [Odd 3, Odd 5 .. Odd 1]

allTests = zip [0..] [test0, test1, test2, test3, test4, test5, test6, test7, test8, test9, test10]

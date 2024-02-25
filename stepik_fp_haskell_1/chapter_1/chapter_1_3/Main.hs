module Main where

infixl 6 *+*
(*+*) :: Num a => a -> a -> a
a *+* b = a^2 + b^2

-- (*+*) a b = a^2 + b^2 -- alternative definition

main :: IO ()
main = return ()

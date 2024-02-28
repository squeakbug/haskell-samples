module Monads.MyStateMonad where

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f k = Reader $ \e ->
            let v = runReader k e
            in f v

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure x = Reader $ const x

instance Monad (Reader r) where
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  m >>= k  = Reader $ \r -> 
             runReader (k (runReader m r)) r

newtype Writer w a = Writer { runWriter :: (a, w) }

newtype State s a = State { runState :: s -> (a, s) }

readerToState :: Reader r a -> State r a
readerToState m = State $ \s ->
    (runReader m s, s)

writerToState :: Monoid w => Writer w a -> State w a
writerToState m = State $ \s ->
    let (a, w) = runWriter m
    in (a, s `mappend` w)

writerToState' :: Monoid w => Writer w a -> State w a
writerToState' m = State $ \s ->
    let (a, w) = runWriter m
    in (a, mempty)

main :: IO ()
main = do
    return ()
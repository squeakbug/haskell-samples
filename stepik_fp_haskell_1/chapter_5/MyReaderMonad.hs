module ReaderMonad where

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

ask :: Reader r r
ask = Reader id

type User = String
type Pass = String
type PassLength = Int
type UserPass = [(User, Pass)]
type UserPassLength = [(User, PassLength)]

pwds :: UserPass
pwds = [("User1", "Pass1"), ("User2", "Pass2")]

firstUser :: Reader UserPass User
firstUser = do
    fst . head <$> ask

lastUser :: Reader UserPass User
lastUser = do
    fst . last <$> ask

local' :: (r -> e) -> Reader e a -> Reader r a
local' f k = Reader $ \r ->
    runReader k (f r)

usersWithBadPasswords :: Reader UserPass [User]
usersWithBadPasswords = do
    users <- ask
    let x = filter (\(uname, upass) -> upass == "123456") users
    return (map fst x)
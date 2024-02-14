module SimpleLexer where

import Data.Char

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken x 
    | all isDigit x = Just (Number (read x))
    | x == "+" = Just Plus
    | x == "-" = Just Minus
    | x == "(" = Just LeftBrace
    | x == ")" = Just RightBrace
    | otherwise = Nothing

tokenize :: String -> Maybe [Token]
tokenize input = foldr f (Just []) (words input) where
    f word maybe_list = do
        token  <- asToken word
        tokens <- maybe_list
        return $ token : tokens

module RemoveFiles where

import System.Directory
import Data.List

removeFiles :: String -> [FilePath] -> IO ()
removeFiles inp = mapM_ (\x -> 
            if inp `isInfixOf` x then do 
                putStrLn ("Removing file: " ++ x)
                removeFile x
            else do
                return ()
        )

main' :: IO ()
main' = do
    putStr "Substring: "
    inp <- getLine
    if inp == "" then
        putStrLn "Canceled"
    else do
        content <- getDirectoryContents "."
        removeFiles inp content

import Control.Monad.Reader

calculateContentLen :: Reader String Int
calculateContentLen = do
    asks length 

calculateContent :: Reader String String
calculateContent = do
    ask

calculateModifiedContentLen :: Reader String Int
calculateModifiedContentLen = local ("prefix: " ++) calculateContentLen

calculateModifiedContent :: Reader String String
calculateModifiedContent = local ("prefix: " ++) calculateContent

addShitContent :: String -> Reader String String
addShitContent oldContent = do
    return ("shit: " ++ oldContent)

main :: IO ()
main = do
    let s = "12345";
    let modifiedLen = runReader calculateModifiedContentLen s
    let modifiedS = runReader (calculateModifiedContent >>= addShitContent >>= addShitContent) s
    let len = runReader calculateContentLen s
    putStrLn $ "Modified 's' length: " ++ show modifiedLen
    putStrLn $ "Original 's' length: " ++ show len
    putStrLn $ "Modified 's': " ++ modifiedS
    putStrLn $ "Original 's': " ++ s
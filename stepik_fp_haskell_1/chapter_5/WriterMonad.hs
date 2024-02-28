import Control.Monad
import Control.Monad.Trans.Writer.Strict

data LogEntry = LogEntry { msg::String }
  deriving (Eq, Show)

calc :: Writer [LogEntry] Integer
calc = do
  output "start"
  let x = sum [1..10000000]
  output (show x)
  output "done"
  return x

output :: String -> Writer [LogEntry] ()
output x = tell [LogEntry x] -- Добавить в лог (окружение) новую запись

main :: IO ()
main = mapM_ print $ execWriter calc
import qualified Data.Text as T
import qualified Mcp
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified System.IO as IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Usage: mcp-cli \"shell command1\" [\"shell command2\" ...]"
      putStrLn "Example: mcp-cli \"ls -l\" \"ps aux\" \"date\""
      exitFailure
    shellCommands -> do
      IO.hSetBuffering IO.stdout IO.LineBuffering
      IO.hSetBuffering IO.stderr IO.LineBuffering
      Mcp.mcpServer $ map T.pack shellCommands

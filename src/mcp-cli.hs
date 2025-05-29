import qualified Data.Text as T
import qualified Mcp
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified System.IO as IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [shellCommand] -> do
      IO.hSetBuffering IO.stdout IO.LineBuffering
      IO.hSetBuffering IO.stderr IO.LineBuffering
      Mcp.mcpServer $ T.pack shellCommand
    _ -> do
      putStrLn "Usage: mcp-cli \"shell command\""
      putStrLn "Example: mcp-cli \"ls -l\""
      exitFailure

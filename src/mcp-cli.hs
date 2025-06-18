import qualified Data.Text as T
import qualified Mcp
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified System.IO as IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> showHelp
    ["-h"] -> showHelp
    [configFile] -> do
      IO.hSetBuffering IO.stdout IO.LineBuffering
      IO.hSetBuffering IO.stderr IO.LineBuffering
      result <- Mcp.loadConfigAndStartServer configFile
      case result of
        Left err -> do
          putStrLn $ "Error: " ++ T.unpack err
          exitFailure
        Right () -> return ()
    _ -> do
      showHelp
      exitFailure

showHelp :: IO ()
showHelp = do
  putStrLn "mcp-cli - Model Context Protocol server for shell commands"
  putStrLn ""
  putStrLn "An MCP tool which exposes the given shell commands as MCP tools."
  putStrLn ""
  putStrLn "USAGE:"
  putStrLn "    mcp-cli mcp-cli.json"
  putStrLn ""
  putStrLn "EXAMPLE mcp-cli.json:"
  putStrLn "    {"
  putStrLn "      \"commands\": ["
  putStrLn "        \"ls -l\","
  putStrLn "        \"date\","
  putStrLn "        \"pwd\","
  putStrLn "        \"whoami\""
  putStrLn "      ]"
  putStrLn "    }"
  putStrLn ""
  putStrLn "EXAMPLE mcp.json (for VS Code):"
  putStrLn "    {"
  putStrLn "      \"servers\": {"
  putStrLn "        \"mcp-cli\": {"
  putStrLn "          \"type\": \"stdio\","
  putStrLn "          \"command\": \"mcp-cli\","
  putStrLn "          \"args\": ["
  putStrLn "            \"${workspaceFolder}/mcp-cli.json\""
  putStrLn "          ]"
  putStrLn "        }"
  putStrLn "      }"
  putStrLn "    }"

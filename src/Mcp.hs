{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Mcp where

import Control.Exception (catch, SomeException)
import Control.Monad (when)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Prelude hiding (id)
import System.Exit (ExitCode (..))
import System.IO (hFlush)
import qualified System.IO as IO
import System.Process.Typed

-- Configuration data types
data CommandConfig = CommandConfig
  { commands :: [Text]
  }
  deriving (Generic, Show)

instance FromJSON CommandConfig

-- JSON-RPC data types
data JsonRpcRequest = JsonRpcRequest
  { jsonrpc :: Text
  , method :: Text
  , params :: Maybe Value
  , id :: Maybe Value
  }
  deriving (Generic, Show)

data JsonRpcResponse = JsonRpcResponse
  { respJsonrpc :: Text
  , result :: Maybe Value
  , respError :: Maybe JsonRpcError
  , respId :: Maybe Value
  }
  deriving (Generic, Show)

data JsonRpcError = JsonRpcError
  { errCode :: Int
  , errMessage :: Text
  , errData :: Maybe Value
  }
  deriving (Generic, Show)

-- MCP-specific data types
data McpTool = McpTool
  { toolName :: Text
  , toolDescription :: Text
  , toolInputSchema :: Value
  }
  deriving (Generic, Show)

data CallToolParams = CallToolParams
  { callToolName :: Text
  , callArguments :: Maybe Value
  }
  deriving (Generic, Show)

-- JSON instances
instance FromJSON JsonRpcRequest where
  parseJSON = withObject "JsonRpcRequest" $ \o -> JsonRpcRequest
    <$> o .: "jsonrpc"
    <*> o .: "method"
    <*> o .:? "params"
    <*> o .:? "id"

instance ToJSON JsonRpcResponse where
  toJSON (JsonRpcResponse rjsonrpc res rerr rid) =
    object $ filter ((/= Null) . snd)
      [ "jsonrpc" .= rjsonrpc
      , "result" .= res
      , "error" .= rerr
      , "id" .= rid
      ]

instance ToJSON JsonRpcError where
  toJSON (JsonRpcError c m d) =
    object $ filter ((/= Null) . snd)
      [ "code" .= c
      , "message" .= m
      , "data" .= d
      ]

instance ToJSON McpTool where
  toJSON (McpTool n d s) =
    object
      [ "name" .= n
      , "description" .= d
      , "inputSchema" .= s
      ]

instance FromJSON CallToolParams where
  parseJSON = withObject "CallToolParams" $ \o -> CallToolParams
    <$> o .: "name"
    <*> o .:? "arguments"

-- Main MCP server function
mcpServer :: [Text] -> IO ()
mcpServer shellCommands = do
  TIO.hPutStrLn IO.stderr "Waiting for connection..."
  hFlush IO.stderr
  serverLoop shellCommands False

serverLoop :: [Text] -> Bool -> IO ()
serverLoop shellCommands connected = do
  line <- TIO.getLine
  case decode $ L8.fromStrict $ Data.Text.Encoding.encodeUtf8 line of
    Nothing -> do
      TIO.hPutStrLn IO.stderr $ "Invalid JSON: " <> line
      serverLoop shellCommands connected
    Just req -> do
      let newConnected = if method req == "initialize" && not connected
                        then True
                        else connected
      when (newConnected && not connected) $
        TIO.hPutStrLn IO.stderr "Connected to VS Code."

      response <- handleRequest shellCommands req
      L8.putStrLn $ encode response
      hFlush IO.stdout
      serverLoop shellCommands newConnected

-- Handle incoming JSON-RPC requests
handleRequest :: [Text] -> JsonRpcRequest -> IO JsonRpcResponse
handleRequest shellCommands req = do
  let requestId = id req
  case method req of
    "initialize" -> return $ JsonRpcResponse "2.0" (Just $ handleInitialize $ params req) Nothing requestId
    "tools/list" -> do
      tools <- handleListTools shellCommands
      return $ JsonRpcResponse "2.0" (Just $ object ["tools" .= tools]) Nothing requestId
    "tools/call" -> do
      res <- case params req of
        Just p -> case fromJSON p of
          Success callParams -> handleCallTool shellCommands callParams
          Error err -> return $ Left $ "Invalid parameters: " <> T.pack err
        Nothing -> return $ Left "Missing parameters"
      case res of
        Right content -> return $ JsonRpcResponse "2.0" (Just content) Nothing requestId
        Left errMsg -> return $ JsonRpcResponse "2.0" Nothing
          (Just $ JsonRpcError (-32602) errMsg Nothing) requestId
    _ -> return $ JsonRpcResponse "2.0" Nothing
      (Just $ JsonRpcError (-32601) "Method not found" Nothing) requestId

-- MCP protocol method implementations
handleInitialize :: Maybe Value -> Value
handleInitialize _ = object
  [ "protocolVersion" .= ("2024-11-05" :: Text)
  , "capabilities" .= object
    [ "tools" .= object []
    ]
  , "serverInfo" .= object
    [ "name" .= ("shell-command-mcp" :: Text)
    , "version" .= ("1.0.0" :: Text)
    ]
  ]

handleListTools :: [Text] -> IO [McpTool]
handleListTools shellCommands = return $
  map (\(i, cmd) -> McpTool
    { toolName = "execute_command_" <> T.pack (show i)
    , toolDescription = "Execute the shell command: " <> cmd
    , toolInputSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object []
      , "required" .= ([] :: [Text])
      ]
    }) (zip [1..] shellCommands)

handleCallTool :: [Text] -> CallToolParams -> IO (Either Text Value)
handleCallTool shellCommands callParams = do
  let toolName = callToolName callParams
  case T.stripPrefix "execute_command_" toolName of
    Just indexText ->
      case reads (T.unpack indexText) of
        [(index, "")] ->
          if index >= 1 && index <= length shellCommands
            then do
              let command = shellCommands !! (index - 1)
              TIO.hPutStrLn IO.stderr $ "> " <> command
              res <- executeShellCommand command
              case res of
                Right (out, err, exitCode) -> return $ Right $ object
                  [ "content" .=
                    [ object
                      [ "type" .= ("text" :: Text)
                      , "text" .= (out <> if T.null err then "" else "\nSTDERR:\n" <> err)
                      ]
                    ]
                  , "isError" .= (exitCode /= 0)
                  ]
                Left e -> return $ Left e
            else return $ Left $ "Invalid command index: " <> T.pack (show index)
        _ -> return $ Left $ "Invalid tool name format: " <> toolName
    Nothing -> return $ Left $ "Unknown tool: " <> toolName

-- Execute shell command using typed-process
executeShellCommand :: Text -> IO (Either Text (Text, Text, Int))
executeShellCommand cmd = do
  res <- catch (tryExecute cmd) handleException
  return res
  where
    tryExecute :: Text -> IO (Either Text (Text, Text, Int))
    tryExecute command = do
      let processConfig = shell $ T.unpack command
      (exitCode, out, err) <- readProcess processConfig
      let exitCodeInt = case exitCode of
            ExitSuccess -> 0
            ExitFailure n -> n
      return $ Right (T.pack $ L8.unpack out, T.pack $ L8.unpack err, exitCodeInt)

    handleException :: SomeException -> IO (Either Text (Text, Text, Int))
    handleException e = return $ Left $ "Failed to execute command: " <> T.pack (show e)

-- Load configuration from JSON file and start MCP server
loadConfigAndStartServer :: FilePath -> IO (Either Text ())
loadConfigAndStartServer configPath = do
  result <- catch (tryLoadConfig configPath) handleFileException
  case result of
    Left err -> return $ Left err
    Right shellCommands -> do
      mcpServer shellCommands
      return $ Right ()
  where
    tryLoadConfig :: FilePath -> IO (Either Text [Text])
    tryLoadConfig path = do
      content <- L.readFile path
      case decode content of
        Nothing -> return $ Left $ "Invalid JSON in config file: " <> T.pack path
        Just config -> return $ Right $ commands config

    handleFileException :: SomeException -> IO (Either Text [Text])
    handleFileException e = return $ Left $ "Failed to read config file: " <> T.pack (show e)

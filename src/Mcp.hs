{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Mcp where

import Control.Applicative ((<|>))
import Control.Exception (catch, SomeException)
import Control.Monad (when)
import Data.Aeson
import Data.Aeson.BetterErrors (Parse)
import qualified Data.Aeson.BetterErrors as ABE
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key (Key, fromText, toText)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Aeson.Key (Key, fromText)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import qualified Prelude (id)
import Prelude hiding (id)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, isAbsolute, (</>))
import System.IO (hFlush, hIsEOF)
import qualified System.IO as IO
import System.Process.Typed
import System.Environment (getEnvironment)

-- Configuration data types
data Argument = Argument
  { argName :: Text
  , argDescription :: Maybe Text
  }
  deriving (Generic, Show)

data Command = Command
  { cmdCommand :: Text
  , cmdName :: Maybe Text
  , cmdDescription :: Maybe Text
  , cmdWorkingDirectory :: Maybe Text
  , cmdArguments :: Maybe [Argument]
  , cmdEnvVars :: Maybe (Map Text (Maybe Text))
  }
  deriving (Generic, Show)

data CommandConfig = CommandConfig
  { commands :: [Command]
  , envVars :: Maybe (Map Text (Maybe Text))
  , workingDirectory :: Maybe Text
  }
  deriving (Generic, Show)

parseArgument :: Parse Text Argument
parseArgument = do
  tp <- ABE.withValue $ \value -> do
    pure $ ABE.jsonTypeOf value
  case tp of
    ABE.TyObject -> do
      name <- ABE.key "name" ABE.asText
      description <- ABE.keyMay "description" ABE.asText
      pure $ Argument name description
    ABE.TyString -> do
      name <- ABE.asText
      pure $ Argument name Nothing
    _ -> do
      ABE.throwCustomError ("Expected object or string, got: " <> T.pack (show tp))

parseEnvVars :: Parse Text (Map Text (Maybe Text))
parseEnvVars = do
  obj <- ABE.asObject
  pure $ Map.fromList $ map parseEnvVar $ KM.toList obj
  where
    parseEnvVar :: (Key, Value) -> (Text, Maybe Text)
    parseEnvVar (key, value) =
      let keyText = toText key
      in case value of
        Null -> (keyText, Nothing)
        String text -> (keyText, Just text)
        _ -> (keyText, Just $ T.pack $ show value)

parseCommand :: Parse Text Command
parseCommand = do
  tp <- ABE.withValue $ \value -> do
    pure $ ABE.jsonTypeOf value
  case tp of
    ABE.TyObject -> do
      cmd <- ABE.key "command" ABE.asText
      name <- ABE.keyMay "name" ABE.asText
      description <- ABE.keyMay "description" ABE.asText
      workingDir <- ABE.keyMay "workingDirectory" ABE.asText
      arguments <- ABE.keyMay "arguments" $ ABE.eachInArray parseArgument
      envVarsMap <- ABE.keyMay "envVars" parseEnvVars
      pure $ Command cmd name description workingDir arguments envVarsMap
    ABE.TyString -> do
      cmd <- ABE.asText
      pure $ Command cmd Nothing Nothing Nothing Nothing Nothing
    _ -> do
      ABE.throwCustomError ("Expected object or string, got: " <> T.pack (show tp))

commandConfigParser :: Parse Text CommandConfig
commandConfigParser = do
  cmds <- ABE.key "commands" $ ABE.eachInArray parseCommand
  globalEnvVars <- ABE.keyMay "envVars" parseEnvVars
  globalWorkingDir <- ABE.keyMay "workingDirectory" ABE.asText
  pure $ CommandConfig cmds globalEnvVars globalWorkingDir

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
mcpServer :: FilePath -> IO ()
mcpServer configPath = do
  TIO.hPutStrLn IO.stderr "Waiting for connection..."
  hFlush IO.stderr
  serverLoop configPath False

serverLoop :: FilePath -> Bool -> IO ()
serverLoop configPath connected = do
  eof <- hIsEOF IO.stdin
  if eof
    then do
      return ()
    else do
      line <- TIO.getLine
      case decode $ L8.fromStrict $ Data.Text.Encoding.encodeUtf8 line of
        Nothing -> do
          TIO.hPutStrLn IO.stderr $ "Invalid JSON: " <> line
          serverLoop configPath connected
        Just req -> do
          let newConnected = if method req == "initialize" && not connected
                            then True
                            else connected
          when (newConnected && not connected) $
            TIO.hPutStrLn IO.stderr "Connected to VS Code."

          response <- handleRequest configPath req
          L8.putStrLn $ encode response
          hFlush IO.stdout
          serverLoop configPath newConnected

-- Handle incoming JSON-RPC requests
handleRequest :: FilePath -> JsonRpcRequest -> IO JsonRpcResponse
handleRequest configPath req = do
  let requestId = id req
  case method req of
    "initialize" -> return $ JsonRpcResponse "2.0" (Just $ handleInitialize $ params req) Nothing requestId
    "tools/list" -> do
      -- Reload configuration file
      configResult <- loadConfig configPath
      case configResult of
        Left err -> return $ JsonRpcResponse "2.0" Nothing
          (Just $ JsonRpcError (-32603) ("Failed to reload config: " <> err) Nothing) requestId
        Right config -> do
          tools <- handleListTools (commands config)
          return $ JsonRpcResponse "2.0" (Just $ object ["tools" .= tools]) Nothing requestId
    "tools/call" -> do
      -- Reload configuration file for tool calls too
      configResult <- loadConfig configPath
      case configResult of
        Left err -> return $ JsonRpcResponse "2.0" Nothing
          (Just $ JsonRpcError (-32603) ("Failed to reload config: " <> err) Nothing) requestId
        Right config -> do
          res <- case params req of
            Just p -> case fromJSON p of
              Success callParams -> handleCallTool configPath config callParams
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

handleListTools :: [Command] -> IO [McpTool]
handleListTools shellCommands = return $
  map (\(i, cmd) -> McpTool
    { toolName = case cmdName cmd of
        Just name -> name
        Nothing -> "execute_command_" <> T.pack (show i)
    , toolDescription = case cmdDescription cmd of
        Just desc -> desc
        Nothing -> "Execute the shell command: " <> cmdCommand cmd
    , toolInputSchema = generateInputSchema $ cmdArguments cmd
    }) (zip [1..] shellCommands)
  where
    generateInputSchema :: Maybe [Argument] -> Value
    generateInputSchema Nothing = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object []
      , "required" .= ([] :: [Text])
      ]
    generateInputSchema (Just args) = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object (map argumentToProperty args)
      , "required" .= map argName args
      ]

    argumentToProperty :: Argument -> (Key, Value)
    argumentToProperty arg =
      ( fromText $ argName arg
      , object $ filter ((/= Null) . snd)
          [ "type" .= ("string" :: Text)
          , "description" .= argDescription arg
          ]
      )

handleCallTool :: FilePath -> CommandConfig -> CallToolParams -> IO (Either Text Value)
handleCallTool configPath config callParams = do
  let toolName = callToolName callParams
      shellCommands = commands config
  -- Try to find command by custom name first
  case findCommandByName toolName shellCommands of
    Just cmd -> executeAndRespond cmd
    Nothing ->
      -- Fall back to the old execute_command_N format
      case T.stripPrefix "execute_command_" toolName of
        Just indexText ->
          case reads (T.unpack indexText) of
            [(index, "")] ->
              if index >= 1 && index <= length shellCommands
                then do
                  let cmd = shellCommands !! (index - 1)
                  executeAndRespond cmd
                else return $ Left $ "Invalid command index: " <> T.pack (show index)
            _ -> return $ Left $ "Invalid tool name format: " <> toolName
        Nothing -> return $ Left $ "Unknown tool: " <> toolName
  where
    findCommandByName :: Text -> [Command] -> Maybe Command
    findCommandByName name cmds =
      case filter (\cmd -> cmdName cmd == Just name) cmds of
        [cmd] -> Just cmd
        _ -> Nothing

    executeAndRespond :: Command -> IO (Either Text Value)
    executeAndRespond cmd = do
      let command = cmdCommand cmd
      TIO.hPutStrLn IO.stderr $ "> " <> command

      -- Extract environment variables from call arguments
      callArgEnvVars <- case callArguments callParams of
        Nothing -> return []
        Just args -> case fromJSON args of
          Success argsObj -> return $ extractEnvVars (cmdArguments cmd) argsObj
          Error _ -> return []

      -- Merge environment variables: global < command-specific < call arguments
      let globalEnvVars = maybe Map.empty Prelude.id $ envVars config
          cmdSpecificEnvVars = maybe Map.empty Prelude.id $ cmdEnvVars cmd
          callArgEnvMap = Map.fromList $ map (\(k, v) -> (k, Just v)) callArgEnvVars
          mergedEnvVars = Map.union callArgEnvMap $ Map.union cmdSpecificEnvVars globalEnvVars

      -- Resolve working directory
      let workingDir = case cmdWorkingDirectory cmd of
            Nothing -> case workingDirectory config of
              Nothing -> Nothing
              Just globalWd -> Just $ resolveWorkingDirectory configPath (T.unpack globalWd)
            Just toolWd -> Just $ resolveToolWorkingDirectory configPath (workingDirectory config) (T.unpack toolWd)

      res <- executeShellCommand workingDir mergedEnvVars command
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

    -- Helper function to extract environment variables from call arguments
    extractEnvVars :: Maybe [Argument] -> Value -> [(Text, Text)]
    extractEnvVars Nothing _ = []
    extractEnvVars (Just args) (Object obj) =
      mapMaybe (\arg ->
        case KM.lookup (fromText $ argName arg) obj of
          Just (String val) -> Just (argName arg, val)
          _ -> Nothing
      ) args
    extractEnvVars _ _ = []

    -- Helper function to resolve working directory paths
    resolveWorkingDirectory :: FilePath -> FilePath -> FilePath
    resolveWorkingDirectory configPath workingDir =
      if isAbsolute workingDir
        then workingDir
        else takeDirectory configPath </> workingDir

    -- Helper function to resolve tool working directory with optional global base
    resolveToolWorkingDirectory :: FilePath -> Maybe Text -> FilePath -> FilePath
    resolveToolWorkingDirectory configPath maybeGlobalWd toolWd =
      case maybeGlobalWd of
        Nothing ->
          -- No global working directory, resolve tool working directory relative to config
          resolveWorkingDirectory configPath toolWd
        Just globalWd ->
          -- Global working directory exists
          let globalResolved = resolveWorkingDirectory configPath (T.unpack globalWd)
          in if isAbsolute toolWd
               then toolWd  -- Tool working directory is absolute, use as-is
               else globalResolved </> toolWd  -- Tool working directory is relative to global

-- Execute shell command using typed-process
executeShellCommand :: Maybe FilePath -> Map Text (Maybe Text) -> Text -> IO (Either Text (Text, Text, Int))
executeShellCommand maybeWorkingDir envVarsMap cmd = do
  res <- catch (tryExecute maybeWorkingDir envVarsMap cmd) handleException
  return res
  where
    tryExecute :: Maybe FilePath -> Map Text (Maybe Text) -> Text -> IO (Either Text (Text, Text, Int))
    tryExecute mWorkingDir envMap command = do
      let baseConfig = shell $ T.unpack command

      let configurePwd :: ProcessConfig stdin stdout stderr
                       -> ProcessConfig stdin stdout stderr
          configurePwd = case mWorkingDir of
            Nothing
              -> Prelude.id
            Just workingDir
              -> setWorkingDir workingDir

      baseVars <- getEnvironment
      -- Convert Map to environment variable list, handling unset variables
      let envVarsList = Map.toList envMap
          setVars = [(T.unpack k, T.unpack v) | (k, Just v) <- envVarsList]
          unsetVars = [T.unpack k | (k, Nothing) <- envVarsList]
          -- Remove unset variables from base environment
          filteredBaseVars = filter (\(k, _) -> k `notElem` unsetVars) baseVars
          allVars = setVars ++ filteredBaseVars
      let configureEnv :: ProcessConfig stdin stdout stderr
                       -> ProcessConfig stdin stdout stderr
          configureEnv = setEnv allVars

      let processConfig = configureEnv . configurePwd $ baseConfig
      (exitCode, out, err) <- readProcess processConfig
      let exitCodeInt = case exitCode of
            ExitSuccess -> 0
            ExitFailure n -> n
      return $ Right (T.pack $ L8.unpack out, T.pack $ L8.unpack err, exitCodeInt)

    handleException :: SomeException -> IO (Either Text (Text, Text, Int))
    handleException e = return $ Left $ "Failed to execute command: " <> T.pack (show e)

-- Load configuration from JSON file
loadConfig :: FilePath -> IO (Either Text CommandConfig)
loadConfig configPath = do
  result <- catch (tryLoadConfig configPath) handleFileException
  return result
  where
    tryLoadConfig :: FilePath -> IO (Either Text CommandConfig)
    tryLoadConfig path = do
      content <- L.readFile path
      case ABE.parse commandConfigParser content of
        Left parseErr -> return $ Left $ formatParseError path parseErr
        Right config -> return $ Right config

    formatParseError :: FilePath -> ABE.ParseError Text -> Text
    formatParseError path parseErr =
      case parseErr of
        ABE.InvalidJSON jsonErr ->
          "Invalid JSON syntax in config file " <> T.pack path <> ": " <> T.pack jsonErr
        ABE.BadSchema _ _ ->
          "Configuration schema error in " <> T.pack path <> ": " <> T.pack (show parseErr)

    handleFileException :: SomeException -> IO (Either Text CommandConfig)
    handleFileException e = return $ Left $ "Failed to read config file: " <> T.pack (show e)

-- Load configuration from JSON file and start MCP server
loadConfigAndStartServer :: FilePath -> IO (Either Text ())
loadConfigAndStartServer configPath = do
  -- Initial config load to validate the file exists and is valid
  result <- loadConfig configPath
  case result of
    Left err -> return $ Left err
    Right _ -> do
      mcpServer configPath
      return $ Right ()

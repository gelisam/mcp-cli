{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Mcp where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (catch, finally, SomeException)
import Control.Monad (when)
import Data.Aeson
import Data.IORef
import Data.Aeson.BetterErrors (Parse)
import Data.Aeson.Key (Key, fromText)
import Data.Aeson.Key (Key, fromText, toText)
import Data.Aeson.Types (Parser)
import Data.Map (Map)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (id)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, isAbsolute, (</>))
import System.IO (hFlush, hIsEOF)
import System.Process.Typed
import qualified Data.Aeson.BetterErrors as ABE
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Prelude (id)
import qualified System.IO as IO
import qualified Data.Aeson as Aeson
import Text.Printf (printf)

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
  , cmdIsRepl :: Bool
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
      isReplMay <- ABE.keyMay "repl" ABE.asBool
      let isRepl = fromMaybe False isReplMay
      pure $ Command cmd name description workingDir arguments envVarsMap isRepl
    ABE.TyString -> do
      cmd <- ABE.asText
      pure $ Command cmd Nothing Nothing Nothing Nothing Nothing False
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

-- REPL management data types
type ReplId = Text

data ReplHandle = ReplHandle
  { replProcess :: Process IO.Handle IO.Handle IO.Handle
  , replStdin :: IO.Handle
  , replStdout :: IO.Handle
  , replStderr :: IO.Handle
  }

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
  Text.hPutStrLn IO.stderr "Waiting for connection..."
  hFlush IO.stderr
  replsRef <- newIORef Map.empty
  -- Cleanup: kill all running REPLs on exit, even if serverLoop throws an exception
  finally
    (serverLoop configPath False replsRef)
    (killAllRepls replsRef)

serverLoop :: FilePath -> Bool -> IORef (Map ReplId ReplHandle) -> IO ()
serverLoop configPath connected replsRef = do
  eof <- hIsEOF IO.stdin
  if eof
    then do
      return ()
    else do
      line <- Text.getLine
      case decode $ LazyByteString.fromStrict $ Text.encodeUtf8 line of
        Nothing -> do
          Text.hPutStrLn IO.stderr $ "Invalid JSON: " <> line
          serverLoop configPath connected replsRef
        Just req -> do
          let newConnected = if method req == "initialize" && not connected
                            then True
                            else connected
          when (newConnected && not connected) $
            Text.hPutStrLn IO.stderr "Connected to VS Code."

          response <- handleRequest configPath replsRef req
          LazyByteString.putStr $ encodeWithUnicodeEscapes response
          Text.putStrLn ""
          hFlush IO.stdout
          serverLoop configPath newConnected replsRef

-- Handle incoming JSON-RPC requests
handleRequest :: FilePath -> IORef (Map ReplId ReplHandle) -> JsonRpcRequest -> IO JsonRpcResponse
handleRequest configPath replsRef req = do
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
              Success callParams -> handleCallTool configPath replsRef config callParams
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
    [ "name" .= ("mcp-cli" :: Text)
    , "version" .= ("1.0.0" :: Text)
    ]
  ]

handleListTools :: [Command] -> IO [McpTool]
handleListTools shellCommands = return $ builtInTools ++ commandTools
  where
    builtInTools =
      [ McpTool
        { toolName = "send_to_repl"
        , toolDescription = "Send a line of input to the REPL with the given id."
        , toolInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
            [ "repl_id" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("The ID of the REPL to send input to, e.g. \"repl-1234\"" :: Text)
              ]
            , "input" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("The line of input to send to the REPL" :: Text)
              ]
            ]
          , "required" .= (["repl_id", "input"] :: [Text])
          ]
        }
      , McpTool
        { toolName = "kill_repl"
        , toolDescription = "Close the stdin of a REPL with the given id, wait a few seconds, and then kill it if it is still running."
        , toolInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
            [ "repl_id" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("The ID of the REPL to kill, e.g. \"repl-1234\"" :: Text)
              ]
            ]
          , "required" .= (["repl_id"] :: [Text])
          ]
        }
      ]
    commandTools = map (\(i, cmd) -> McpTool
      { toolName = case cmdName cmd of
          Just name -> name
          Nothing -> "execute_command_" <> T.pack (show i)
      , toolDescription =
          let baseDesc = case cmdDescription cmd of
                Just desc -> desc
                Nothing -> "Execute the shell command: " <> cmdCommand cmd
              replSuffix = if cmdIsRepl cmd
                            then "\n\nThis is a REPL tool. The output will be an ID of the form `repl-1234` which can be used with the send_to_repl, read_from_repl, and kill_repl commands."
                            else ""
          in baseDesc <> replSuffix
      , toolInputSchema = generateInputSchema $ cmdArguments cmd
      }) (zip [1..] shellCommands)

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

handleCallTool :: FilePath -> IORef (Map ReplId ReplHandle) -> CommandConfig -> CallToolParams -> IO (Either Text Value)
handleCallTool configPath replsRef config callParams = do
  let toolName = callToolName callParams
      shellCommands = commands config
  -- Check if this is a built-in tool first
  case toolName of
    "send_to_repl" -> handleSendToRepl replsRef callParams
    "kill_repl" -> handleKillRepl replsRef callParams
    _ -> do
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
      Text.hPutStrLn IO.stderr $ "> " <> command

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

      -- Handle REPL tools differently
      if cmdIsRepl cmd
        then do
          res <- startRepl replsRef workingDir mergedEnvVars command
          case res of
            Right replId -> return $ Right $ object
              [ "content" .=
                [ object
                  [ "type" .= ("text" :: Text)
                  , "text" .= replId
                  ]
                ]
              , "isError" .= False
              ]
            Left e -> return $ Left e
        else do
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

-- Helper function to configure process working directory
configureWorkingDir :: Maybe FilePath -> ProcessConfig stdin stdout stderr -> ProcessConfig stdin stdout stderr
configureWorkingDir mWorkingDir = case mWorkingDir of
  Nothing -> Prelude.id
  Just workingDir -> setWorkingDir workingDir

-- Helper function to configure process environment variables
configureEnvVars :: Map Text (Maybe Text) -> ProcessConfig stdin stdout stderr -> IO (ProcessConfig stdin stdout stderr)
configureEnvVars envMap baseConfig = do
  baseVars <- getEnvironment
  -- Convert Map to environment variable list, handling unset variables
  let envVarsList = Map.toList envMap
      setVars = [(T.unpack k, T.unpack v) | (k, Just v) <- envVarsList]
      unsetVars = [T.unpack k | (k, Nothing) <- envVarsList]
      -- Remove unset variables from base environment
      filteredBaseVars = filter (\(k, _) -> k `notElem` unsetVars) baseVars
      allVars = setVars ++ filteredBaseVars
  return $ setEnv allVars baseConfig

-- Execute shell command using typed-process
executeShellCommand :: Maybe FilePath -> Map Text (Maybe Text) -> Text -> IO (Either Text (Text, Text, Int))
executeShellCommand maybeWorkingDir envVarsMap cmd = do
  res <- catch (tryExecute maybeWorkingDir envVarsMap cmd) handleException
  return res
  where
    tryExecute :: Maybe FilePath -> Map Text (Maybe Text) -> Text -> IO (Either Text (Text, Text, Int))
    tryExecute mWorkingDir envMap command = do
      let baseConfig = shell $ T.unpack command
          withWorkingDir = configureWorkingDir mWorkingDir baseConfig
      processConfig <- configureEnvVars envMap withWorkingDir
      (exitCode, out, err) <- readProcess processConfig
      let exitCodeInt = case exitCode of
            ExitSuccess -> 0
            ExitFailure n -> n
      let outText = LazyText.decodeUtf8 out
          errText = LazyText.decodeUtf8 err
      return $ Right (LazyText.toStrict outText, LazyText.toStrict errText, exitCodeInt)

    handleException :: SomeException -> IO (Either Text (Text, Text, Int))
    handleException e = return $ Left $ "Failed to execute command: " <> T.pack (show e)

-- Kill all running REPLs (used on shutdown)
killAllRepls :: IORef (Map ReplId ReplHandle) -> IO ()
killAllRepls replsRef = do
  repls <- readIORef replsRef
  let replIds = Map.keys repls
  when (not (null replIds)) $ do
    Text.hPutStrLn IO.stderr $ "Shutting down, killing " <> T.pack (show (length replIds)) <> " REPL(s)..."
    mapM_ (\replId -> do
      result <- killRepl replsRef replId
      case result of
        Left err -> Text.hPutStrLn IO.stderr $ "Warning: Failed to kill REPL " <> replId <> ": " <> err
        Right msg -> Text.putStrLn msg
      ) replIds

-- Kill a REPL process: close stdin, wait, and kill if still running
killRepl :: IORef (Map ReplId ReplHandle) -> ReplId -> IO (Either Text Text)
killRepl replsRef replId = do
  repls <- readIORef replsRef
  case Map.lookup replId repls of
    Nothing -> return $ Left $ "Unknown REPL id: " <> replId
    Just replHandle -> do
      res <- catch (tryKillRepl replHandle) handleException
      -- Remove from map regardless of success
      modifyIORef replsRef $ Map.delete replId
      return res
  where
    tryKillRepl :: ReplHandle -> IO (Either Text Text)
    tryKillRepl replHandle = do
      -- Close stdin
      IO.hClose (replStdin replHandle)

      -- Wait 3 seconds (3000000 microseconds)
      threadDelay 3000000

      -- Check if process is still running and kill it
      let process = replProcess replHandle
      maybeExitCode <- getExitCode process
      case maybeExitCode of
        Just _ ->
          -- Process already exited
          return $ Right $ "REPL " <> replId <> " has been stopped"
        Nothing -> do
          -- Process still running, need to kill it
          stopProcess process
          return $ Right $ "REPL " <> replId <> " has been killed"

    handleException :: SomeException -> IO (Either Text Text)
    handleException e = return $ Left $ "Failed to kill REPL: " <> T.pack (show e)

-- Handle send_to_repl tool call
handleSendToRepl :: IORef (Map ReplId ReplHandle) -> CallToolParams -> IO (Either Text Value)
handleSendToRepl replsRef callParams = do
  case callArguments callParams of
    Just (Object obj) -> do
      case (KM.lookup "repl_id" obj, KM.lookup "input" obj) of
        (Just (String replId), Just (String input)) -> do
          res <- sendToRepl replsRef replId input
          case res of
            Right () -> return $ Right $ object
              [ "content" .= ([] :: [Value])
              , "isError" .= False
              ]
            Left err -> return $ Left err
        _ -> return $ Left "Missing or invalid repl_id or input"
    _ -> return $ Left "Missing or invalid arguments"

-- Send a line of input to a REPL process
sendToRepl :: IORef (Map ReplId ReplHandle) -> ReplId -> Text -> IO (Either Text ())
sendToRepl replsRef replId input = do
  repls <- readIORef replsRef
  case Map.lookup replId repls of
    Nothing -> return $ Left $ "Unknown REPL id: " <> replId
    Just replHandle -> do
      res <- catch (trySendToRepl replHandle) handleException
      return res
  where
    trySendToRepl :: ReplHandle -> IO (Either Text ())
    trySendToRepl replHandle = do
      -- Write the input line to the REPL's stdin
      Text.hPutStrLn (replStdin replHandle) input
      IO.hFlush (replStdin replHandle)
      return $ Right ()

    handleException :: SomeException -> IO (Either Text ())
    handleException e = return $ Left $ "Failed to send to REPL: " <> T.pack (show e)

-- Handle kill_repl tool call
handleKillRepl :: IORef (Map ReplId ReplHandle) -> CallToolParams -> IO (Either Text Value)
handleKillRepl replsRef callParams = do
  case callArguments callParams of
    Just (Object obj) -> do
      case KM.lookup "repl_id" obj of
        Just (String replId) -> do
          res <- killRepl replsRef replId
          case res of
            Right msg -> return $ Right $ object
              [ "content" .=
                [ object
                  [ "type" .= ("text" :: Text)
                  , "text" .= msg
                  ]
                ]
              , "isError" .= False
              ]
            Left err -> return $ Left err
        _ -> return $ Left "repl_id must be a string"
    _ -> return $ Left "Missing or invalid arguments"

-- Start a REPL process and return its ID
startRepl :: IORef (Map ReplId ReplHandle) -> Maybe FilePath -> Map Text (Maybe Text) -> Text -> IO (Either Text ReplId)
startRepl replsRef maybeWorkingDir envVarsMap cmd = do
  res <- catch (tryStartRepl maybeWorkingDir envVarsMap cmd) handleException
  return res
  where
    tryStartRepl :: Maybe FilePath -> Map Text (Maybe Text) -> Text -> IO (Either Text ReplId)
    tryStartRepl mWorkingDir envMap command = do
      let baseConfig = shell $ T.unpack command
          withWorkingDir = configureWorkingDir mWorkingDir baseConfig
      withEnvVars <- configureEnvVars envMap withWorkingDir
      let processConfig = setStdin createPipe
                        $ setStdout createPipe
                        $ setStderr createPipe
                        $ withEnvVars

      process <- startProcess processConfig
      let stdinH = getStdin process
          stdoutH = getStdout process
          stderrH = getStderr process

      -- Set non-blocking mode for stdout and stderr
      IO.hSetBuffering stdinH IO.LineBuffering
      IO.hSetBuffering stdoutH IO.NoBuffering
      IO.hSetBuffering stderrH IO.NoBuffering

      -- Generate unique REPL ID
      repls <- readIORef replsRef
      let replId = "repl-" <> T.pack (show (Map.size repls + 1))

      -- Store REPL handle
      let replHandle = ReplHandle
            { replProcess = process
            , replStdin = stdinH
            , replStdout = stdoutH
            , replStderr = stderrH
            }
      modifyIORef replsRef $ Map.insert replId replHandle

      return $ Right replId

    handleException :: SomeException -> IO (Either Text ReplId)
    handleException e = return $ Left $ "Failed to start REPL: " <> T.pack (show e)

-- Load configuration from JSON file
loadConfig :: FilePath -> IO (Either Text CommandConfig)
loadConfig configPath = do
  result <- catch (tryLoadConfig configPath) handleFileException
  return result
  where
    tryLoadConfig :: FilePath -> IO (Either Text CommandConfig)
    tryLoadConfig path = do
      content <- LazyByteString.readFile path
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

-- Custom JSON encoder that forces Unicode escapes for non-ASCII characters
encodeWithUnicodeEscapes :: ToJSON a => a -> LazyByteString.ByteString
encodeWithUnicodeEscapes value =
  let jsonText = LazyText.decodeUtf8 $ encode value
      escapedText = LazyText.concatMap escapeChar jsonText
  in LazyText.encodeUtf8 escapedText
  where
    escapeChar :: Char -> LazyText.Text
    escapeChar c
      | c >= '\x80' = LazyText.pack $ "\\u" ++ printf "%04x" (fromEnum c)
      | otherwise = LazyText.singleton c

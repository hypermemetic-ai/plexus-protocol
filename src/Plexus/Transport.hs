-- | Low-level transport for Substrate RPC calls
--
-- Pure IO functions for WebSocket communication with the Substrate backend.
-- All calls go through '<backend>.call' for routing.
module Plexus.Transport
  ( -- * RPC Calls (collected)
    rpcCall
  , rpcCallWith

    -- * RPC Calls (streaming)
  , rpcCallStreaming
  , invokeMethodStreaming
  , invokeMethodStreamingWith
  , invokeMethodDuplex


    -- * Schema Fetching
  , fetchSchemaAt
  , fetchSchemaAtIfAny
  , schemaNotFound
  , fetchMethodSchemaAt
  , extractSchema
  , extractSchemaResult

    -- * Method Invocation (collected)
  , invokeMethod
  , invokeRaw

    -- * Routing a call elsewhere (SYNAPSE-28)
  , CallRoute
  , setCallRouter
  , DuplexRoute
  , setDuplexRouter
  ) where

import Control.Exception (SomeException, IOException, catch, fromException)
import qualified Control.Exception as E
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.Aeson hiding (Error)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Streaming.Prelude as S
import qualified Network.Socket as NS
import System.IO.Unsafe (unsafePerformIO)

import Plexus.Client (SubstrateConfig(..), LostConnection(..), connect, disconnect, substrateRpc, substrateRpcDuplex, defaultConfig)
import Plexus.Input (InputHalf)
import Plexus.Client.Pool (ConnectionPool, createConnectionPool, withPooledConnection, defaultPlexusPoolConfig)
import Plexus.RemoteError (RemoteError(..), renderRemoteError)
import Plexus.Types (PlexusStreamItem(..), TransportError(..))
import Plexus.Schema.Recursive (PluginSchema, MethodSchema, SchemaResult(..), parsePluginSchema, parseSchemaResult)

-- | Global connection pool cache
-- One pool per unique SubstrateConfig to enable connection reuse across calls
{-# NOINLINE poolCache #-}
poolCache :: MVar (Map SubstrateConfig ConnectionPool)
poolCache = unsafePerformIO (newMVar Map.empty)

-- | Get or create a connection pool for the given config
getOrCreatePool :: SubstrateConfig -> IO ConnectionPool
getOrCreatePool cfg = modifyMVar poolCache $ \pools ->
  case Map.lookup cfg pools of
    Just pool -> pure (pools, pool)
    Nothing -> do
      pool <- createConnectionPool cfg defaultPlexusPoolConfig
      pure (Map.insert cfg pool pools, pool)

-- | SYNAPSE-28: one call, made some other way than a pooled connection of
-- this process — @onSubscribed method params onItem@ — e.g. through a
-- connection held between runs, or one connection a whole process shares.
-- It reports a lost connection as 'Plexus.Client.connect' and
-- 'substrateRpcWith' do ('LostConnection', a failed dial as an error
-- call), so everything above it reads its failures as before.
type CallRoute = IO () -> Text -> Value -> (PlexusStreamItem -> IO ()) -> IO ()

-- | BIDIR-18: a route that can also carry a call's input half
-- (plexus-wire spec/10): given 'Just' a half, it hands the half the call's
-- 'Plexus.Input.InputPort' once the subscription is known and its signals
-- as they come; given 'Nothing', it is a 'CallRoute'.
type DuplexRoute = Maybe InputHalf -> CallRoute

{-# NOINLINE routerRef #-}
routerRef :: IORef (SubstrateConfig -> Maybe DuplexRoute)
routerRef = unsafePerformIO (newIORef (const Nothing))

-- | SYNAPSE-28: route the calls whose config the router answers 'Just'
-- for; the rest go through this process's pools, as before. Set once, at
-- start (a program that never sets it is unchanged). A route set here
-- cannot carry an input half: a call with one fails on it, naming why
-- ('setDuplexRouter' sets one that can).
setCallRouter :: (SubstrateConfig -> Maybe CallRoute) -> IO ()
setCallRouter f = writeIORef routerRef (fmap plainOnly . f)
  where
    plainOnly r Nothing = r
    plainOnly _ (Just _) = \_ _ _ _ -> E.throwIO (userError "this call route cannot carry a call's input half")

-- | BIDIR-18: 'setCallRouter' with routes that carry an input half too.
setDuplexRouter :: (SubstrateConfig -> Maybe DuplexRoute) -> IO ()
setDuplexRouter = writeIORef routerRef

-- | Low-level RPC call with default localhost config
rpcCall :: Text -> Text -> Value -> IO (Either TransportError [PlexusStreamItem])
rpcCall backend = rpcCallWith (defaultConfig backend)

-- | Low-level RPC call with custom config
rpcCallWith :: SubstrateConfig -> Text -> Value -> IO (Either TransportError [PlexusStreamItem])
rpcCallWith cfg method params = do
  result <- (Right <$> doCallInner cfg method params)
    `catch` categorizeException cfg
  pure result

-- | Categorize exceptions into typed TransportError
categorizeException :: SubstrateConfig -> SomeException -> IO (Either TransportError a)
categorizeException cfg e
  -- SYNAPSE-20: the connection went while the call was on it
  | Just (LostConnection why sent) <- fromException e =
      pure $ Left $ ConnectionLost why sent
  -- Check for connection refused (most common)
  | Just ioErr <- fromException e :: Maybe IOException = do
      let errMsg = show ioErr
      let host = T.pack $ substrateHost cfg
      let port = substratePort cfg
      pure $ Left $ if "refused" `isInfixOf` errMsg || "ECONNREFUSED" `isInfixOf` errMsg
        then ConnectionRefused host port
        else if "timeout" `isInfixOf` errMsg || "ETIMEDOUT" `isInfixOf` errMsg
          then ConnectionTimeout host port
          else NetworkError (T.pack errMsg)

  -- Catch protocol/parse errors
  | otherwise =
      let errMsg = show e
          host = T.pack $ substrateHost cfg
          port = substratePort cfg
      in pure $ Left $
        -- SYNAPSE-20: 'connect' reports a refused or timed-out dial as an
        -- error call; say which, so a caller can tell an outage from a fault
        if "refused" `isInfixOf` errMsg || "ECONNREFUSED" `isInfixOf` errMsg
          then ConnectionRefused host port
        else if "Connection timeout" `isInfixOf` errMsg
          then ConnectionTimeout host port
        else if "protocol" `isInfixOf` errMsg || "parse" `isInfixOf` errMsg
        then ProtocolError (T.pack errMsg)
        else NetworkError (T.pack errMsg)

  where
    isInfixOf = T.isInfixOf `on` (T.toLower . T.pack)
    on f g x y = f (g x) (g y)

doCallInner :: SubstrateConfig -> Text -> Value -> IO [PlexusStreamItem]
doCallInner cfg method params = do
  router <- readIORef routerRef
  case router cfg of
    Just route -> do
      acc <- newIORef []
      route Nothing (pure ()) method params (\i -> modifyIORef' acc (i :))
      reverse <$> readIORef acc
    Nothing -> freshIfUnsent $ do
      pool <- getOrCreatePool cfg
      withPooledConnection pool $ \conn ->
        S.toList_ $ substrateRpc conn method params

-- | SYNAPSE-20: a pooled connection that died while idle fails the call
-- before anything is sent (and the pool drops it); the call goes once more,
-- on a fresh connection. A call whose request went out is never repeated
-- here — whether that is safe is the caller's to say.
freshIfUnsent :: IO a -> IO a
freshIfUnsent act = act `E.catch` \(e :: LostConnection) ->
  if lostSent e then E.throwIO e else act

-- | Streaming RPC call - invokes callback for each item as it arrives
rpcCallStreaming :: SubstrateConfig -> Text -> Value -> (PlexusStreamItem -> IO ()) -> IO (Either TransportError ())
rpcCallStreaming cfg method params onItem = do
  result <- (Right <$> doCallStreaming cfg method params onItem)
    `catch` categorizeException cfg
  pure result

doCallStreaming :: SubstrateConfig -> Text -> Value -> (PlexusStreamItem -> IO ()) -> IO ()
doCallStreaming cfg = doCallStreamingWith cfg (pure ())

doCallStreamingWith :: SubstrateConfig -> IO () -> Text -> Value -> (PlexusStreamItem -> IO ()) -> IO ()
doCallStreamingWith cfg = doCallDuplex cfg Nothing

doCallDuplex :: SubstrateConfig -> Maybe InputHalf -> IO () -> Text -> Value -> (PlexusStreamItem -> IO ()) -> IO ()
doCallDuplex cfg half onSubscribed method params onItem = do
  router <- readIORef routerRef
  case router cfg of
    Just route -> route half onSubscribed method params onItem
    Nothing -> freshIfUnsent $ do
      pool <- getOrCreatePool cfg
      withPooledConnection pool $ \conn ->
        S.mapM_ onItem $ substrateRpcDuplex conn half (const onSubscribed) method params

-- | Streaming method invocation
invokeMethodStreaming :: SubstrateConfig -> [Text] -> Text -> Value -> (PlexusStreamItem -> IO ()) -> IO (Either TransportError ())
invokeMethodStreaming cfg = invokeMethodStreamingWith cfg (pure ())

-- | SYNAPSE-20: 'invokeMethodStreaming', telling @onSubscribed@ when the
-- server has taken the call, before any item.
invokeMethodStreamingWith :: SubstrateConfig -> IO () -> [Text] -> Text -> Value -> (PlexusStreamItem -> IO ()) -> IO (Either TransportError ())
invokeMethodStreamingWith cfg onSubscribed namespacePath method params onItem = do
  let backend = substrateBackend cfg
  let fullPath = if null namespacePath then [backend] else namespacePath
  let dotPath = T.intercalate "." (fullPath ++ [method])
  let callParams = object ["method" .= dotPath, "params" .= params]
  (Right <$> doCallStreamingWith cfg onSubscribed (backend <> ".call") callParams onItem)
    `catch` categorizeException cfg

-- | BIDIR-18: 'invokeMethodStreamingWith' for a method with an input half:
-- the half is handed its port once the server has taken the call (before
-- @onSubscribed@) and the callee's credit, acks and close as they come.
-- Nothing here sends the call twice: a call whose request went out and
-- whose connection went is 'ConnectionLost' with @sent@.
invokeMethodDuplex :: SubstrateConfig -> InputHalf -> IO () -> [Text] -> Text -> Value -> (PlexusStreamItem -> IO ()) -> IO (Either TransportError ())
invokeMethodDuplex cfg half onSubscribed namespacePath method params onItem = do
  let backend = substrateBackend cfg
  let fullPath = if null namespacePath then [backend] else namespacePath
  let dotPath = T.intercalate "." (fullPath ++ [method])
  let callParams = object ["method" .= dotPath, "params" .= params]
  (Right <$> doCallDuplex cfg (Just half) onSubscribed (backend <> ".call") callParams onItem)
    `catch` categorizeException cfg

-- | Fetch schema at a specific path
-- Empty path = root (<backend>.schema)
-- Non-empty path = child schema (e.g., ["solar", "earth"] -> solar.earth.schema)
fetchSchemaAt :: SubstrateConfig -> [Text] -> IO (Either TransportError PluginSchema)
fetchSchemaAt cfg path = do
  let backend = substrateBackend cfg
  let schemaMethod = if null path
        then backend <> ".schema"
        else T.intercalate "." path <> ".schema"
  result <- rpcCallWith cfg (backend <> ".call") (object ["method" .= schemaMethod])
  case result of
    Left transportErr -> pure $ Left transportErr
    Right items -> case extractSchema items of
      Left parseErr -> pure $ Left $ ProtocolError parseErr  -- Parse errors are protocol errors
      Right schema -> pure $ Right schema

-- | NEST-5 / NEST-7: the schema at a path that may not be there — a name
--   in a slot (a dynamic child's key, a peer's name). @Right Nothing@ when
--   the service says there is nothing at the path (activation or method not
--   found, as a call failure or a JSON-RPC @-32601@; a slot's own
--   @not_found@, as plexus-core's @PeerError::NotFound@); every other failure
--   (the connection, a refusal, an unreadable answer) is @Left@, as
--   'fetchSchemaAt' says it.
fetchSchemaAtIfAny :: SubstrateConfig -> [Text] -> IO (Either TransportError (Maybe PluginSchema))
fetchSchemaAtIfAny cfg path = do
  let backend = substrateBackend cfg
  let schemaMethod = if null path
        then backend <> ".schema"
        else T.intercalate "." path <> ".schema"
  result <- rpcCallWith cfg (backend <> ".call") (object ["method" .= schemaMethod])
  pure $ case result of
    Left transportErr -> Left transportErr
    Right items
      | schemaNotFound items -> Right Nothing
      | otherwise -> either (Left . ProtocolError) (Right . Just) (extractSchema items)

-- | Whether a schema call's items say there is nothing at its path: no
--   schema, and the first failure is activation or method not found, or
--   a slot's @not_found@.
schemaNotFound :: [PlexusStreamItem] -> Bool
schemaNotFound items =
  null [() | StreamData _ _ ct _ <- items, ".schema" `T.isSuffixOf` ct]
    && case [err | StreamError _ _ err _ <- items] of
         (ActivationNotFound _ : _) -> True
         (MethodNotFound _ _ : _)   -> True
         (Other "not_found" _ : _)  -> True
         _                          -> False

-- | Extract PluginSchema from stream items
-- Returns Either Text for application-level errors (not transport)
extractSchema :: [PlexusStreamItem] -> Either Text PluginSchema
extractSchema items =
  case [dat | StreamData _ _ ct dat <- items, ".schema" `T.isSuffixOf` ct] of
    (dat:_) -> parsePluginSchema dat
    [] -> case [renderRemoteError err | StreamError _ _ err _ <- items] of
      (err:_) -> Left err
      [] -> Left "No schema in response"

-- | Fetch a specific method's schema
-- Uses the parameter-based query: plugin.schema with {"method": "name"}
fetchMethodSchemaAt :: SubstrateConfig -> [Text] -> Text -> IO (Either TransportError MethodSchema)
fetchMethodSchemaAt cfg path methodName = do
  let backend = substrateBackend cfg
  let schemaMethod = if null path
        then backend <> ".schema"
        else T.intercalate "." path <> ".schema"
  result <- rpcCallWith cfg (backend <> ".call") (object
    [ "method" .= schemaMethod
    , "params" .= object ["method" .= methodName]
    ])
  case result of
    Left transportErr -> pure $ Left transportErr
    Right items -> case extractSchemaResult items of
      Left parseErr -> pure $ Left $ ProtocolError parseErr
      Right (SchemaMethod m) -> pure $ Right m
      Right (SchemaPlugin _) -> pure $ Left $ ProtocolError "Expected method schema, got plugin schema"

-- | Extract SchemaResult (plugin or method) from stream items
extractSchemaResult :: [PlexusStreamItem] -> Either Text SchemaResult
extractSchemaResult items =
  case [dat | StreamData _ _ ct dat <- items, ".schema" `T.isSuffixOf` ct] of
    (dat:_) -> parseSchemaResult dat
    [] -> case [renderRemoteError err | StreamError _ _ err _ <- items] of
      (err:_) -> Left err
      [] -> Left "No schema in response"

-- | Invoke a method and return stream items
invokeMethod :: SubstrateConfig -> [Text] -> Text -> Value -> IO (Either TransportError [PlexusStreamItem])
invokeMethod cfg namespacePath method params = do
  let backend = substrateBackend cfg
  let fullPath = if null namespacePath then [backend] else namespacePath
  let dotPath = T.intercalate "." (fullPath ++ [method])
  let callParams = object ["method" .= dotPath, "params" .= params]
  rpcCallWith cfg (backend <> ".call") callParams

-- | Invoke with raw method path
invokeRaw :: SubstrateConfig -> Text -> Value -> IO (Either TransportError [PlexusStreamItem])
invokeRaw cfg method params = do
  let backend = substrateBackend cfg
  let callParams = object ["method" .= method, "params" .= params]
  rpcCallWith cfg (backend <> ".call") callParams

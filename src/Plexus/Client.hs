-- | Low-level WebSocket client for Substrate
--
-- The core primitive is:
--
-- @
-- substrateRpc :: SubstrateConnection -> Value -> Stream (Of PlexusStreamItem) IO ()
-- @
--
-- This establishes a subscription and yields stream items until completion.
module Plexus.Client
  ( -- * Connection
    SubstrateConnection
  , connect
  , disconnect
  , connectionLost

    -- * A lost connection (SYNAPSE-20)
  , LostConnection(..)
  , Keepalive(..)
  , keepaliveFromEnv

    -- * Core RPC primitive
  , substrateRpc
  , substrateRpcWith

    -- * Configuration
  , SubstrateConfig(..)
  , defaultConfig
  , cookieHeader
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Async (Async, async, cancel, wait)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, catch, fromException, throwIO, finally)
import Control.Monad (forever, void, when, unless)
import GHC.Clock (getMonotonicTime)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Network.WebSockets (Connection)
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import Streaming
import qualified Streaming.Prelude as Str

import Plexus.Types

-- | Substrate connection configuration
data SubstrateConfig = SubstrateConfig
  { substrateHost    :: String
  , substratePort    :: Int
  , substratePath    :: String
  , substrateBackend :: Text    -- ^ Backend name (e.g., "plexus")
  , substrateHeaders :: WS.Headers  -- ^ Extra HTTP upgrade headers (e.g., Cookie)
  }
  deriving stock (Show, Eq, Ord)

-- | Default configuration for local development (requires backend)
defaultConfig :: Text -> SubstrateConfig
defaultConfig backend = SubstrateConfig
  { substrateHost    = "127.0.0.1"
  , substratePort    = 4444
  , substratePath    = "/"
  , substrateBackend = backend
  , substrateHeaders = []
  }

-- | Build a Cookie header from a JWT token
cookieHeader :: Text -> WS.Headers
cookieHeader tok = [(CI.mk "Cookie", "access_token=" <> BS8.pack (T.unpack tok))]

-- | Pending request with its queue, waiting for subscription ID
data PendingRequest = PendingRequest
  { prQueue    :: TQueue Value        -- ^ Queue to receive notifications
  , prResponse :: TMVar RpcResponse   -- ^ Response channel
  }

-- | Active connection to substrate
data SubstrateConnection = SubstrateConnection
  { scConnection    :: Connection
  , scNextId        :: IORef Int
  , scSubscriptions :: TVar (Map SubscriptionId (TQueue Value))
  , scPendingReqs   :: TVar (Map RequestId PendingRequest)
  , scReaderThread  :: Async ()
  , scClosed        :: TMVar Text
    -- ^ SYNAPSE-20: filled, once, with why the connection is gone — the
    -- reader ended (a close, a reset) or a keepalive ping went unanswered.
    -- Every call waiting on this connection waits on it too, so none is
    -- left waiting on a dead socket.
  , scKeepalive     :: Maybe (Async ())
  }

-- | SYNAPSE-20: a call's connection was lost. 'lostSent' says whether the
-- call's request had gone out before it was.
data LostConnection = LostConnection
  { lostReason :: Text
  , lostSent   :: Bool
  }
  deriving stock (Show)

instance Exception LostConnection

-- | SYNAPSE-20: how a silent drop (no close, no reset: a peer that stopped,
-- a network that went away) is noticed. After 'kaIdle' seconds with
-- nothing heard, a ping is sent; when nothing (a pong or any message) is
-- heard 'kaTimeout' seconds after it, the connection is lost.
data Keepalive = Keepalive
  { kaIdle    :: Double
  , kaTimeout :: Double
  }
  deriving stock (Show, Eq)

-- | The keepalive: @PLEXUS_KEEPALIVE=IDLE[,TIMEOUT]@ in seconds, @0@ or
-- @off@ for none; by default a ping after 15 s of silence, lost 10 s later.
keepaliveFromEnv :: IO (Maybe Keepalive)
keepaliveFromEnv = do
  v <- lookupEnv "PLEXUS_KEEPALIVE"
  pure $ case fmap (T.splitOn "," . T.strip . T.pack) v of
    Nothing -> Just def
    Just [t] | t `elem` ["0", "off", "none"] -> Nothing
    Just [i] | Just i' <- num i, i' > 0 -> Just def { kaIdle = i' }
    Just [i, t] | Just i' <- num i, Just t' <- num t, i' > 0, t' > 0 -> Just (Keepalive i' t')
    Just _ -> Just def
  where
    def = Keepalive 15 10
    num :: Text -> Maybe Double
    num t = case readMaybe (T.unpack (T.strip t)) :: Maybe Double of
      Just d -> Just d
      Nothing -> fromIntegral <$> (readMaybe (T.unpack (T.strip t)) :: Maybe Int)

-- | SYNAPSE-20: why the connection is gone, if it is.
connectionLost :: SubstrateConnection -> IO (Maybe Text)
connectionLost c = atomically (tryReadTMVar (scClosed c))

-- | Say why the connection ended, from what ended the reader.
describeEnd :: SomeException -> Text
describeEnd e = case fromException e of
  Just (WS.CloseRequest code msg) ->
    "the server closed the connection (" <> T.pack (show code)
      <> (if LBS.null msg then "" else ": " <> T.pack (LBS.unpack msg)) <> ")"
  Just WS.ConnectionClosed -> "the connection was closed"
  Just other -> "the connection failed: " <> T.pack (show other)
  Nothing -> "the connection failed: " <> T.pack (show e)

-- | Connect to substrate
-- Throws an exception if connection fails
connect :: SubstrateConfig -> IO SubstrateConnection
connect SubstrateConfig{..} = do
  -- Initialize state
  nextId <- newIORef 1
  subs   <- newTVarIO Map.empty
  pendingReqs <- newTVarIO Map.empty
  closed <- newEmptyTMVarIO
  resultVar <- newEmptyTMVarIO  -- Either error message or (conn, reader)
  ka <- keepaliveFromEnv
  -- SYNAPSE-20: when anything was last heard (a message or a pong)
  heard <- newIORef =<< getMonotonicTime
  let touch = getMonotonicTime >>= writeIORef heard
      lose why = atomically (void (tryPutTMVar closed why))
      opts = WS.defaultConnectionOptions { WS.connectionOnPong = touch }

  -- Run WebSocket client in a background thread
  -- Catch exceptions inside the thread to prevent them from being printed
  tid <- forkIO $
    (WS.runClientWith substrateHost substratePort substratePath
        opts substrateHeaders $ \conn -> do
      reader <- async $
        readerLoop conn touch subs pendingReqs
          `catch` (\(e :: SomeException) -> lose (describeEnd e))
      -- Signal success
      atomically $ putTMVar resultVar (Right (conn, reader))
      -- Keep alive until reader exits
      void (wait reader) `catch` \(_ :: SomeException) -> pure ()
    ) `catch` (\(e :: SomeException) ->
      -- Signal failure (don't print, just capture)
      atomically $ void $ tryPutTMVar resultVar (Left $ show e))
      `finally` lose "the connection was closed"

  -- Wait for connection result (with 5s timeout instead of hardcoded 200ms sleep)
  mResult <- timeout 5000000 $ atomically $ takeTMVar resultVar

  case mResult of
    Nothing -> killThread tid >> error "Connection timeout (5s)"
    Just (Left err) -> error $ "Connection failed: " <> err
    Just (Right (conn, reader)) -> do
      kat <- traverse (\k -> async (keepaliveLoop k conn heard closed reader)) ka
      pure SubstrateConnection
        { scConnection    = conn
        , scNextId        = nextId
        , scSubscriptions = subs
        , scPendingReqs   = pendingReqs
        , scReaderThread  = reader
        , scClosed        = closed
        , scKeepalive     = kat
        }

-- | SYNAPSE-20: ping after 'kaIdle' seconds of silence; when nothing is
-- heard 'kaTimeout' seconds after the ping, the connection is lost — the
-- waiting calls are told and the reader is stopped, which closes the socket.
keepaliveLoop :: Keepalive -> Connection -> IORef Double -> TMVar Text -> Async () -> IO ()
keepaliveLoop Keepalive{..} conn heard closed reader = go Nothing
  where
    tick = min 1 (kaIdle / 2)
    go pinged = do
      threadDelay (round (tick * 1000000))
      gone <- atomically (not <$> isEmptyTMVar closed)
      unless gone $ do
        now <- getMonotonicTime
        h <- readIORef heard
        case pinged of
          Just at | h < at && now - at >= kaTimeout -> do
            atomically $ void $ tryPutTMVar closed $
              "no answer to a keepalive ping in " <> T.pack (show (round kaTimeout :: Int)) <> " s"
            cancel reader
          Just at | h < at -> go pinged
          _ | now - h >= kaIdle -> do
                WS.sendPing conn ("synapse" :: Text)
                  `catch` \(_ :: SomeException) -> pure ()
                go (Just now)
            | otherwise -> go Nothing

-- | Disconnect from substrate
disconnect :: SubstrateConnection -> IO ()
disconnect SubstrateConnection{..} = do
  mapM_ cancel scKeepalive
  cancel scReaderThread
  WS.sendClose scConnection ("bye" :: Text) `catch` \(_ :: SomeException) -> pure ()

-- | Reader loop - dispatches incoming messages to the right handler
readerLoop
  :: Connection
  -> IO ()
  -> TVar (Map SubscriptionId (TQueue Value))
  -> TVar (Map RequestId PendingRequest)
  -> IO ()
readerLoop conn touch subs pendingReqs = forever $ do
  msg <- WS.receiveData conn
  touch
  case eitherDecode msg of
    Left err -> putStrLn $ "Failed to decode message: " <> err
    Right val -> dispatch val
  where
    dispatch :: Value -> IO ()
    dispatch val = case val of
      Object o
        -- Check if it's a subscription notification (has "params.subscription" but no "id")
        | Just (Object params) <- KM.lookup "params" o
        , Just _ <- KM.lookup "subscription" params
        , Nothing <- KM.lookup "id" o
        -> handleNotification val

        -- Otherwise it's a response (has "id")
        | Just _ <- KM.lookup "id" o
        -> handleResponse val

      _ -> putStrLn $ "Unknown message format: " <> show val

    handleNotification :: Value -> IO ()
    handleNotification val =
      case fromJSON val of
        Success (SubscriptionNotification _ _ params) -> do
          let subId = subParamsSubscription params
              result = subParamsResult params
          mQueue <- Map.lookup subId <$> readTVarIO subs
          case mQueue of
            Just queue -> atomically $ writeTQueue queue result
            Nothing    -> putStrLn $ "Unknown subscription: " <> show subId
        Error err -> putStrLn $ "Failed to parse notification: " <> err

    handleResponse :: Value -> IO ()
    handleResponse val =
      case fromJSON val of
        Success resp -> do
          let rid = rpcRespId resp
          mPending <- Map.lookup rid <$> readTVarIO pendingReqs
          case mPending of
            Just (PendingRequest queue respVar) -> do
              -- For successful responses, register the subscription queue BEFORE
              -- signaling completion. This prevents race with notifications.
              case resp of
                RpcSuccess _ result ->
                  case fromJSON result of
                    Success subId -> atomically $ do
                      modifyTVar' subs $ Map.insert subId queue
                      putTMVar respVar resp
                    Error _ ->
                      -- Can't parse subId, just signal response
                      atomically $ putTMVar respVar resp
                RpcError{} ->
                  atomically $ putTMVar respVar resp
            Nothing -> putStrLn $ "Unknown request id: " <> show rid
        Error err -> putStrLn $ "Failed to parse response: " <> err

-- | The core RPC primitive
--
-- @substrateRpc conn method params@ sends a subscription request and returns a stream
-- of 'PlexusStreamItem' values. The stream completes when a 'StreamDone' or
-- 'StreamError' item is received.
--
-- Example:
--
-- @
-- import qualified Streaming.Prelude as S
--
-- main = do
--   conn <- connect defaultConfig
--   S.print $ substrateRpc conn "bash_execute" (toJSON ["echo hello"])
-- @
substrateRpc
  :: SubstrateConnection
  -> Text              -- ^ Method name (e.g., "bash_execute")
  -> Value             -- ^ Parameters
  -> Stream (Of PlexusStreamItem) IO ()
substrateRpc conn = substrateRpcWith conn (pure ())

-- | SYNAPSE-20: 'substrateRpc', telling @onSubscribed@ when the server has
-- taken the call (its subscription confirmed) — before any item.
substrateRpcWith
  :: SubstrateConnection
  -> IO ()
  -> Text
  -> Value
  -> Stream (Of PlexusStreamItem) IO ()
substrateRpcWith conn onSubscribed method params = do
  -- SYNAPSE-20: a connection already gone (it died idle in a pool) takes
  -- no request; nothing was sent, so the caller may always try again
  gone <- liftIO $ connectionLost conn
  case gone of
    Just why -> liftIO $ throwIO (LostConnection why False)
    Nothing -> pure ()

  -- Get next request ID
  rid <- liftIO $ atomicModifyIORef' (scNextId conn) $ \n -> (n + 1, RequestId n)

  -- Create queue for this subscription and response channel
  queue <- liftIO newTQueueIO
  respVar <- liftIO newEmptyTMVarIO

  -- Register pending request (includes queue so reader can register subscription)
  liftIO $ atomically $ modifyTVar' (scPendingReqs conn) $
    Map.insert rid (PendingRequest queue respVar)

  -- Send subscription request (a send that fails sent nothing)
  let req = mkSubscribeRequest rid method params
  liftIO $ WS.sendTextData (scConnection conn) (encode req)
    `catch` \(e :: SomeException) -> do
      why <- maybe (describeEnd e) id <$> connectionLost conn
      throwIO (LostConnection why False)

  -- Wait for subscription confirmation — or for the connection to go
  resp <- liftIO $ atomically $
    (Right <$> takeTMVar respVar) `orElse` (Left <$> readTMVar (scClosed conn))

  -- Clean up pending request
  liftIO $ atomically $ modifyTVar' (scPendingReqs conn) $ Map.delete rid

  case resp of
    Left why -> liftIO $ throwIO (LostConnection why True)

    Right (RpcError _ err) -> do
      -- Emit error as StreamError so caller can handle it properly
      Str.yield $ StreamErrorWith
        { itemPlexusHash = ""  -- No hash available for subscription errors
        , itemProvenance = Provenance ["substrate"]
        , itemError = T.pack $ "Subscription error: " <> show err
        , itemRecoverable = False
        , itemErrorRefusal = Nothing
        }

    Right (RpcSuccess _ result) -> do
      -- The reader loop already registered the queue in scSubscriptions
      -- when it processed the response, so we just need the subId for cleanup
      case fromJSON result of
        Error err -> do
          liftIO $ putStrLn $ "Failed to parse subscription id: " <> err
          pure ()

        Success subId -> do
          liftIO onSubscribed
          -- Stream items until done, cleanup when finished
          streamItems queue subId <* liftIO (cleanup subId)
  where
    -- SYNAPSE-20: what arrived before the connection went is delivered
    -- first ('orElse' tries the queue first); then the call fails with
    -- 'LostConnection' instead of waiting forever
    next :: TQueue Value -> IO Value
    next queue = do
      r <- atomically $
        (Right <$> readTQueue queue) `orElse` (Left <$> readTMVar (scClosed conn))
      either (\why -> throwIO (LostConnection why True)) pure r

    streamItems :: TQueue Value -> SubscriptionId -> Stream (Of PlexusStreamItem) IO ()
    streamItems queue subId = do
      val <- liftIO $ next queue
      case fromJSON val of
        Error err -> do
          liftIO $ putStrLn $ "Failed to parse stream item: " <> err
          streamItems queue subId

        Success item -> do
          Str.yield item
          case item of
            StreamDone{}  -> pure ()  -- End of stream
            StreamError{} -> pure ()  -- Error terminates stream
            _             -> streamItems queue subId

    cleanup :: SubscriptionId -> IO ()
    cleanup subId = atomically $ modifyTVar' (scSubscriptions conn) $
      Map.delete subId

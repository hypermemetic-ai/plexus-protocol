-- | Plexus RPC Types and Client
--
-- Re-exports from Plexus (transport and client) and Plexus (RPC types).
--
-- = Quick Start
--
-- @
-- import Plexus
-- import qualified Streaming.Prelude as S
-- import Data.Aeson (toJSON)
--
-- main :: IO ()
-- main = do
--   -- Connect to substrate
--   conn <- connect defaultConfig
--
--   -- Call bash.execute and stream results
--   S.print $ substrateRpc conn "bash_execute" (toJSON ["echo hello"])
--
--   -- Clean up
--   disconnect conn
-- @
--
-- = Stream Items
--
-- The Plexus RPC protocol returns a unified stream type with variants:
--
-- * 'StreamProgress' - Progress updates with optional percentage
-- * 'StreamData' - Actual data with content type information
-- * 'StreamError' - Error (may be recoverable)
-- * 'StreamDone' - Stream completed successfully
--
module Plexus
  ( -- * Connection (re-exported from Plexus.Client)
    SubstrateConnection
  , SubstrateConfig(..)
  , TlsOptions(..)
  , defaultTlsOptions
  , defaultConfig
  , connect
  , disconnect

    -- * Core RPC (streaming)
  , substrateRpc

    -- * High-level RPC (collected, re-exported from Plexus.Transport)
  , rpcCall
  , rpcCallWith
  , fetchSchemaAt
  , invokeMethod
  , invokeRaw

    -- * High-level RPC (streaming)
  , rpcCallStreaming
  , invokeMethodStreaming

    -- * Bidirectional Response
  , sendBidirectionalResponse

    -- * Bidirectional Types
  , StandardRequest(..)
  , StandardResponse(..)
  , SelectOption(..)

    -- * Stream Types
  , PlexusStreamItem(..)
  , Provenance(..)

    -- * Schema Types
  , PluginSchema(..)
  , MethodSchema(..)
  , ChildSummary(..)
  , PluginHash
  , SchemaResult(..)

    -- * JSON-RPC Types
  , RpcRequest(..)
  , RpcResponse(..)
  , RpcError(..)
  , RequestId(..)
  , SubscriptionId(..)
  ) where

import Plexus.Types
import Plexus.Schema.Recursive
import Plexus.Client
import Plexus.Transport

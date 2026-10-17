# plexus-protocol

**Haskell types for Plexus RPC** — shared between Synapse and other Plexus RPC clients.

## Layers

```
┌─────────────────────────────────────────┐
│  Plexus.Types                           │  Stream items, provenance
│  Plexus.Schema.Recursive                │  PluginSchema, MethodSchema
├─────────────────────────────────────────┤
│  Substrate.Transport                    │  RPC calls, schema fetching
│  Substrate.Client                       │  WebSocket connection
└─────────────────────────────────────────┘
```

**Substrate** = transport layer modules (WebSocket, JSON-RPC)
**Plexus** = Plexus RPC protocol types (schemas, stream items)

## Usage

```haskell
import Plexus
import qualified Streaming.Prelude as S

main :: IO ()
main = do
  conn <- connect defaultConfig
  S.print $ substrateRpc conn "health.check" (object [])
  disconnect conn
```

### wss (SCAFFOLD-21, 0.5.2.0)

A hub behind TLS (a hosted app: `wss://<host>/ws`) is dialed when
`substrateTls` is `Just`: the `tls` package over a TCP socket, SNI and the
name check against `substrateHost`, the chain against the system's trust
store (crypton-x509-system; `SYSTEM_CERTIFICATE_PATH` overrides it) plus each
PEM file in `tlsCaFiles`. Verification cannot be turned off. `defaultConfig`
is `Nothing` (plain `ws://`), as before; the upgrade's `Host` is `hostHeader`
(the host alone on the scheme's default port).

```haskell
let cfg = (defaultConfig "hostdemo")
      { substrateHost = "hostdemo.menger.sh", substratePort = 443
      , substratePath = "/ws", substrateTls = Just defaultTlsOptions }
Right items <- rpcCallWith cfg "_info" Null
```

### Schema Fetching

```haskell
import Substrate.Transport

-- Fetch plugin schema
Right schema <- fetchSchemaAt defaultConfig ["echo"]
print $ psNamespace schema  -- "echo"
print $ psMethods schema    -- [MethodSchema...]

-- Fetch method schema directly
Right method <- fetchMethodSchemaAt defaultConfig ["echo"] "once"
print $ methodName method   -- "once"
print $ methodParams method -- Just (JSON Schema)
```

## Types

### Transport Errors (v0.3.0+)

Strongly-typed errors for transport-layer failures:

```haskell
data TransportError
  = ConnectionRefused { transportHost :: Text, transportPort :: Int }
  | ConnectionTimeout { transportHost :: Text, transportPort :: Int }
  | ProtocolError     { transportMessage :: Text }
  | NetworkError      { transportMessage :: Text }
```

All transport functions return `Either TransportError a` instead of `Either Text a`:

```haskell
-- Schema fetching
fetchSchemaAt :: SubstrateConfig -> [Text] -> IO (Either TransportError PluginSchema)

-- Method invocation
invokeMethod :: SubstrateConfig -> [Text] -> Text -> Value
             -> IO (Either TransportError [PlexusStreamItem])

-- Streaming invocation
invokeMethodStreaming :: SubstrateConfig -> [Text] -> Text -> Value
                      -> (PlexusStreamItem -> IO ())
                      -> IO (Either TransportError ())
```

**Benefits:**
- No string parsing to categorize errors
- Structured error information (host, port, message)
- Type-safe error handling
- Proper exception categorization using `IOException` matching

**Example:**

```haskell
result <- fetchSchemaAt cfg ["echo"]
case result of
  Left (ConnectionRefused host port) ->
    putStrLn $ "Can't connect to " <> host <> ":" <> show port
  Left (ConnectionTimeout host port) ->
    putStrLn $ "Timeout connecting to " <> host <> ":" <> show port
  Left (ProtocolError msg) ->
    putStrLn $ "Protocol error: " <> msg
  Left (NetworkError msg) ->
    putStrLn $ "Network error: " <> msg
  Right schema ->
    print schema
```

### Stream Items

```haskell
data PlexusStreamItem
  = StreamProgress Provenance PluginHash Text (Maybe Int)
  | StreamData Provenance PluginHash Text Value
  | StreamError Provenance PluginHash Text Bool
  | StreamDone Provenance PluginHash
```

### Schema Types

```haskell
data PluginSchema = PluginSchema
  { psNamespace       :: Text
  , psVersion         :: Text
  , psDescription     :: Text
  , psLongDescription :: Maybe Text
  , psHash            :: PluginHash
  , psMethods         :: [MethodSchema]
  , psChildren        :: Maybe [ChildSummary]
  }

data MethodSchema = MethodSchema
  { methodName        :: Text
  , methodDescription :: Text
  , methodHash        :: PluginHash
  , methodParams      :: Maybe Value  -- JSON Schema
  , methodReturns     :: Maybe Value  -- JSON Schema
  }

data SchemaResult
  = SchemaPlugin PluginSchema
  | SchemaMethod MethodSchema
```

## Structure

```
src/
├── Plexus.hs                   # Re-exports
├── Plexus/
│   ├── Types.hs                # PlexusStreamItem, Provenance
│   ├── Schema.hs               # Schema re-exports
│   └── Schema/
│       ├── Recursive.hs        # PluginSchema, MethodSchema, SchemaResult
│       └── Cache.hs            # Schema caching
└── Substrate/
    ├── Client.hs               # WebSocket connection
    └── Transport.hs            # RPC calls, schema fetching
```

## Build

```bash
cabal build
cabal test
```

## License

MIT

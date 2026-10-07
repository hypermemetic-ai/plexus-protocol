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

### Stream Items (plexus 0.7)

```haskell
data PlexusStreamItem
  = StreamProgress Text Provenance Text Value        -- hash, provenance, content_type, content (the method's progress P)
  | StreamData     Text Provenance Text Value        -- hash, provenance, content_type, content (T, or {"ok": T} | {"err": E})
  | StreamGuidance …
  | StreamError    Text Provenance RemoteError Bool  -- hash, provenance, error, recoverable
  | StreamRequest  …
  | StreamDone     Text Provenance
```

An error frame means the call itself failed, and says why with a
`RemoteError` (`Plexus.RemoteError`, mirroring `plexus_client::RemoteError`;
`renderRemoteError` is its one-line rendering). A JSON-RPC rejection carries
the same type in `errData`. An activation saying no is a data item whose
content is `{"err": …}`.

```haskell
data RemoteError
  = ActivationNotFound Text            -- activation
  | MethodNotFound Text Text           -- activation, method
  | InvalidParams (Maybe Text) ParamProblem
  | Unauthenticated
  | Forbidden DenyReason
  | HandleNotSupported Text            -- activation
  | Execution Text                     -- message

data ParamProblem = Missing | Malformed Text
data DenyReason   = MissingScope Text | TenantBoundary | NotAccepted
```

### Schema Types

```haskell
data PluginSchema = PluginSchema
  { psNamespace       :: Text
  , psVersion         :: Text
  , psDescription     :: Text
  , psLongDescription :: Maybe Text
  , psSelfHash        :: Maybe PluginHash
  , psHash            :: PluginHash
  , psMethods         :: [MethodSchema]
  , psChildren        :: Maybe [ChildSummary]
  , psDeprecation     :: Maybe DeprecationInfo
  }

-- plexus 0.7: a method's contract is T, E?, P? and its cardinality
data MethodSchema = MethodSchema
  { methodName        :: Text
  , methodDescription :: Text
  , methodHash        :: PluginHash
  , methodParams      :: Maybe Value  -- JSON Schema
  , methodReturns     :: Value        -- JSON Schema of the item type T (required)
  , methodError       :: Maybe Value  -- JSON Schema of the error type E (fallible methods)
  , methodProgress    :: Maybe Value  -- JSON Schema of the progress type P
  , methodCardinality :: Cardinality  -- CardinalityOne | CardinalityMany (required)
  , …                                 -- bidirectional, http_method, request/response types, deprecation, role
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

The tests read the plexus 0.7 conformance fixture (RETURNS-7) by sibling
path, `../plexus-macros/fixtures/conformance/expected/`: check out
plexus-macros next to this repository first.

## License

MIT

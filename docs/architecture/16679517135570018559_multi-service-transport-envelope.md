# Multi-Service Architecture with PlexusStreamItem Transport Envelope

## TL;DR

**Goal:** Unify the wire format and naming conventions across all services, preparing for multi-backend architecture.

**Key Changes:**
1. `plexus_call` → `{backend}.call` (backend-namespaced dot notation)
2. Synapse CLI: `synapse echo once` → `synapse plexus echo once` (explicit backend)
3. Remove `CallEvent` wrapper - return `PlexusStreamItem` directly from `service.call`
4. Every response on the wire is `PlexusStreamItem` (uniform envelope)

**Affected Codebases:**
| Codebase | Changes |
|----------|---------|
| plexus-core | Remove CallEvent, plexus.call returns PlexusStreamItem directly |
| plexus-macros | Rename generated RPC from `_` to `.` notation |
| substrate | Update RPC registration for dot notation |
| substrate-protocol | Change `"plexus_call"` → `"{backend}.call"` in Transport.hs |
| synapse | Add backend as first path segment, remove CallEvent unwrap |
| service-codegen | Add PlexusStreamItem to IR, two-layer generation |

**Wire Format (after changes):**
```json
// Request
{"jsonrpc": "2.0", "method": "{backend}.call", "params": {"method": "echo.once", "params": {"message": "hi"}}, "id": 1}

// Response (uniform PlexusStreamItem envelope)
{"type": "data", "content_type": "echo.once", "content": {...}, "metadata": {"provenance": ["echo"], "plexus_hash": "...", "timestamp": ...}}
{"type": "done", "metadata": {...}}
```

**CLI (after changes):**
```bash
synapse plexus echo once --message hi      # explicit backend
synapse plexus solar earth luna info       # nested routing
synapse otherservice some method               # future: other backends
```

**PlexusStreamItem (the universal envelope):**
```rust
pub enum PlexusStreamItem {
    Data {
        metadata: StreamMetadata,
        content_type: String,      // fully qualified: "solar.earth.luna.info"
        content: Value,            // the domain data (EchoEvent, HealthEvent, etc.)
    },
    Progress {
        metadata: StreamMetadata,
        message: String,
        percentage: Option<f32>,
    },
    Error {
        metadata: StreamMetadata,
        message: String,
        code: Option<String>,
    },
    Done {
        metadata: StreamMetadata,
    },
}

pub struct StreamMetadata {
    pub provenance: Vec<String>,   // routing path: ["solar", "earth", "luna"]
    pub plexus_hash: String,       // schema version hash
    pub timestamp: u64,
}
```

**Current problem being solved:**
- `{backend}.call` returns `CallEvent` which wraps `PlexusStreamItem` content
- This creates double-wrapping: domain → PlexusStreamItem → CallEvent → PlexusStreamItem
- Synapse has special unwrap logic for CallEvent
- Old `plexus_call` used underscore while everything else uses dots (now standardized to `{backend}.call`)

---

## Vision

Move from a single Plexus service to multiple spawnable service backends, each serving as an independent routing layer. All services share a common wire format: `PlexusStreamItem`.

## Namespace Consistency

### RPC Method Naming

Change from underscore to backend-namespaced dot notation for consistency:

```
# Before (inconsistent - old underscore notation)
plexus_call, plexus_schema, plexus_hash

# After (consistent backend-namespaced dot notation)
{backend}.call, {backend}.schema, {backend}.hash

# For plexus backend specifically:
plexus.call, plexus.schema, plexus.hash
```

All methods follow `namespace.method` pattern uniformly, with the backend name as the namespace.

### Synapse CLI - Explicit Backend Namespacing

Synapse should treat backends as first-class namespaces:

```bash
# Current: plexus is implicit
synapse echo once --message hi
synapse solar earth luna info

# Proposed: backend is explicit
synapse plexus echo once --message hi
synapse plexus solar earth luna info

# Future: multiple backends
synapse plexus echo once --message hi     # plexus backend on :4444
synapse otherservice foo bar                   # different backend on :5555
synapse arbor-cluster node list            # another backend
```

### Backend Discovery

A service can host other backends as subplugins while those backends remain independently accessible:

```
┌─────────────────────────────────────────────────┐
│  Orchestrator Service                               │
│  Exposes: backends.list, backends.info          │
│  Returns connection info for each backend       │
├─────────────────────────────────────────────────┤
│  backends.info("plexus")                        │
│  → { "host": "localhost", "port": 4444,         │
│      "protocol": "ws", "namespace": "plexus" }  │
├─────────────────────────────────────────────────┤
│  backends.info("arbor-cluster")                 │
│  → { "host": "10.0.0.5", "port": 8080,          │
│      "protocol": "ws", "namespace": "arbor" }   │
└─────────────────────────────────────────────────┘
```

Synapse can:
1. Connect to orchestrator, discover backends
2. Connect directly to backends for performance
3. Route through orchestrator for convenience

### Wire Format with Explicit Backend

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "{backend}.call",
  "params": {
    "method": "echo.once",
    "params": { "message": "hello" }
  }
}
```

Or direct method call (still routed through `{backend}.call` internally):
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "echo.once",
  "params": { "message": "hello" }
}
```

## Current State

```
┌─────────────────────────────────────┐
│  Synapse / Client                   │
│  Unwraps CallEvent → PlexusStreamItem│
├─────────────────────────────────────┤
│  {backend}.call                     │
│  Returns Stream<CallEvent>          │
│  (CallEvent wraps PlexusStreamItem) │
├─────────────────────────────────────┤
│  Activations                        │
│  Return Stream<DomainEvent>         │
│  wrap_stream → PlexusStreamItem     │
└─────────────────────────────────────┘
```

**Problems:**
- `CallEvent` is redundant (PlexusStreamItem minus metadata)
- Double-wrapping: DomainEvent → PlexusStreamItem → CallEvent → PlexusStreamItem
- Only specific backends have `call` - needs to be uniform across services
- Synapse needs special unwrap logic for CallEvent

## Target Architecture

```
┌─────────────────────────────────────────────────────────┐
│  Native Client Layer                                    │
│  Typed methods: echo.once({...}): Promise<EchoEvent>    │
│  Generated wrappers unwrap PlexusStreamItem.Data.content│
├─────────────────────────────────────────────────────────┤
│  RPC Transport Layer                                    │
│  service.call(method, params): AsyncGenerator<PlexusStreamItem>│
│  Uniform across all services                                │
├─────────────────────────────────────────────────────────┤
│  Service Backend (spawnable, one of many)                   │
│  Routes to activations                                  │
│  All responses are PlexusStreamItem                     │
└─────────────────────────────────────────────────────────┘
```

## Core Changes

### 1. PlexusStreamItem as Universal Transport Envelope

Every method, at the wire level, returns `Stream<PlexusStreamItem>`:

```rust
pub enum PlexusStreamItem {
    Data {
        metadata: StreamMetadata,  // provenance, hash, timestamp
        content_type: String,      // "echo.once", "solar.earth.info"
        content: Value,            // the actual domain data
    },
    Progress { metadata, message, percentage },
    Error { metadata, message, code },
    Done { metadata },
}
```

This is already true - `wrap_stream` produces this. The change is making it explicit in the type system and IR.

### 2. Remove CallEvent

`{backend}.call` (any service's `call` method) returns `PlexusStreamItem` directly:

```rust
// Before
async fn call(&self, method: String, params: Value) -> impl Stream<Item = CallEvent>

// After
async fn call(&self, method: String, params: Value) -> impl Stream<Item = PlexusStreamItem>
```

Implementation forwards the routed stream without re-wrapping:

```rust
async fn call(&self, method: String, params: Option<Value>) -> impl Stream<Item = PlexusStreamItem> {
    match self.route(&method, params.unwrap_or_default()).await {
        Ok(stream) => stream,  // forward directly
        Err(e) => stream::once(async move {
            PlexusStreamItem::Error {
                metadata: StreamMetadata::now(),
                message: e.to_string(),
                code: None,
            }
        }).boxed(),
    }
}
```

### 3. Every Service Exposes `call`

The `call` method becomes universal to all plugins that can route:

| Plugin Type | Has `call` | Behavior |
|-------------|------------|----------|
| Leaf (echo, health) | No | Direct method invocation only |
| Parent (solar, plexus) | Yes | Routes to children via `call` |

Parents implement `ChildRouter` and expose `call`. The macro could auto-generate this for `parent = true` plugins.

### 4. IR Changes

PlexusStreamItem becomes a first-class type:

```json
{
  "types": {
    "PlexusStreamItem": {
      "kind": "discriminated_union",
      "tag": "type",
      "variants": {
        "data": { "metadata": "StreamMetadata", "content_type": "string", "content": "unknown" },
        "progress": { "metadata": "StreamMetadata", "message": "string", "percentage": "number?" },
        "error": { "metadata": "StreamMetadata", "message": "string", "code": "string?" },
        "done": { "metadata": "StreamMetadata" }
      }
    }
  },
  "methods": {
    "plexus.call": {
      "params": { "method": "string", "params": "unknown?" },
      "returns": "PlexusStreamItem",
      "streaming": true
    },
    "echo.once": {
      "params": { "message": "string" },
      "returns": "EchoEvent",      // domain type
      "streaming": false
    }
  }
}
```

### 5. Two-Layer Code Generation

**Layer 1: RPC Client (raw)**
```typescript
interface ServiceRpcClient {
  call(method: string, params?: unknown): AsyncGenerator<PlexusStreamItem>;
}
```

**Layer 2: Native Client (typed)**
```typescript
interface EchoClient {
  once(params: { message: string }): Promise<EchoEvent>;
}

// Generated implementation
class EchoClientImpl implements EchoClient {
  constructor(private rpc: ServiceRpcClient) {}

  async once(params: { message: string }): Promise<EchoEvent> {
    const stream = this.rpc.call("echo.once", params);
    for await (const item of stream) {
      if (item.type === "data") {
        return item.content as EchoEvent;
      }
      if (item.type === "error") {
        throw new Error(item.message);
      }
    }
    throw new Error("No data received");
  }
}
```

For streaming methods:
```typescript
interface ConeClient {
  chat(params: ChatParams): AsyncGenerator<ChatEvent>;
}

// Generated - yields unwrapped content
async *chat(params: ChatParams): AsyncGenerator<ChatEvent> {
  for await (const item of this.rpc.call("cone.chat", params)) {
    if (item.type === "data") yield item.content as ChatEvent;
    if (item.type === "error") throw new Error(item.message);
  }
}
```

## Multi-Service Spawning

Each service backend is an independent process:

```
┌──────────────┐  ┌──────────────┐  ┌──────────────┐
│  Service A       │  │  Service B       │  │  Service C       │
│  :4444       │  │  :4445       │  │  :4446       │
│  - echo      │  │  - cone      │  │  - arbor     │
│  - health    │  │  - claudecode│  │  - changelog │
└──────────────┘  └──────────────┘  └──────────────┘
       │                 │                 │
       └────────────┬────┴─────────────────┘
                    │
            ┌───────────────┐
            │  Orchestrator │
            │  Routes to    │
            │  appropriate  │
            │  service backend  │
            └───────────────┘
```

Each service:
- Has its own `call` method
- Returns `PlexusStreamItem` uniformly
- Can be spawned/scaled independently
- Shares the same wire format

## Wire Format Examples

### Example 1: Direct Method Call

**Request:** `echo.once`
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "{backend}.call",
  "params": {
    "method": "echo.once",
    "params": { "message": "hello" }
  }
}
```

**Response stream:**
```json
{"jsonrpc": "2.0", "method": "subscription", "params": {"subscription": "sub_1", "result":
  {"type": "data", "content_type": "echo.once", "content": {"event": "echo", "message": "hello", "count": 1}, "metadata": {"provenance": ["echo"], "plexus_hash": "abc123", "timestamp": 1234567890}}
}}
{"jsonrpc": "2.0", "method": "subscription", "params": {"subscription": "sub_1", "result":
  {"type": "done", "metadata": {"provenance": ["echo"], "plexus_hash": "abc123", "timestamp": 1234567890}}
}}
```

### Example 2: One-Level Nesting (Parent → Child)

**Request:** `solar.observe`
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "{backend}.call",
  "params": {
    "method": "solar.observe",
    "params": {}
  }
}
```

**Response:**
```json
{"type": "data", "content_type": "solar.observe", "content": {"planets": ["mercury", "venus", "earth", ...]}, "metadata": {"provenance": ["solar"], "plexus_hash": "abc123", "timestamp": 1234567890}}
{"type": "done", "metadata": {"provenance": ["solar"], "plexus_hash": "abc123", "timestamp": 1234567890}}
```

### Example 3: Two-Level Nesting (Parent → Child → Grandchild)

**Request:** `solar.earth.info`
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "{backend}.call",
  "params": {
    "method": "solar.earth.info",
    "params": {}
  }
}
```

**Response:**
```json
{"type": "data", "content_type": "solar.earth.info", "content": {"name": "Earth", "type": "planet", "mass": 5.97e24}, "metadata": {"provenance": ["solar", "earth"], "plexus_hash": "abc123", "timestamp": 1234567890}}
{"type": "done", "metadata": {"provenance": ["solar", "earth"], "plexus_hash": "abc123", "timestamp": 1234567890}}
```

### Example 4: Three-Level Nesting (Parent → Child → Grandchild → Method)

**Request:** `solar.earth.luna.info`
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "{backend}.call",
  "params": {
    "method": "solar.earth.luna.info",
    "params": {}
  }
}
```

**Response:**
```json
{"type": "data", "content_type": "solar.earth.luna.info", "content": {"name": "Luna", "type": "moon", "parent": "Earth"}, "metadata": {"provenance": ["solar", "earth", "luna"], "plexus_hash": "abc123", "timestamp": 1234567890}}
{"type": "done", "metadata": {"provenance": ["solar", "earth", "luna"], "plexus_hash": "abc123", "timestamp": 1234567890}}
```

### Example 5: Streaming Method (Multiple Data Events)

**Request:** `cone.chat`
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "{backend}.call",
  "params": {
    "method": "cone.chat",
    "params": { "identifier": "my-cone", "prompt": "Hello!" }
  }
}
```

**Response stream:**
```json
{"type": "progress", "message": "Thinking...", "percentage": null, "metadata": {"provenance": ["cone"], ...}}
{"type": "data", "content_type": "cone.chat", "content": {"type": "token", "text": "Hello"}, "metadata": {"provenance": ["cone"], ...}}
{"type": "data", "content_type": "cone.chat", "content": {"type": "token", "text": " there"}, "metadata": {"provenance": ["cone"], ...}}
{"type": "data", "content_type": "cone.chat", "content": {"type": "token", "text": "!"}, "metadata": {"provenance": ["cone"], ...}}
{"type": "data", "content_type": "cone.chat", "content": {"type": "complete", "node_id": "uuid-123"}, "metadata": {"provenance": ["cone"], ...}}
{"type": "done", "metadata": {"provenance": ["cone"], ...}}
```

### Example 6: Error Response

**Request:** Invalid method
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "{backend}.call",
  "params": {
    "method": "nonexistent.method",
    "params": {}
  }
}
```

**Response:**
```json
{"type": "error", "message": "Activation not found: nonexistent", "code": null, "metadata": {"provenance": ["plexus"], "plexus_hash": "abc123", "timestamp": 1234567890}}
{"type": "done", "metadata": {"provenance": ["plexus"], ...}}
```

### Key Observations

1. **Uniform envelope**: Every response is `PlexusStreamItem` regardless of nesting depth
2. **Provenance tracks path**: `["solar", "earth", "luna"]` shows the routing chain
3. **content_type is fully qualified**: `"solar.earth.luna.info"` not just `"info"`
4. **No double-wrapping**: Single `PlexusStreamItem` layer, content is domain data
5. **Errors are stream events**: Not JSON-RPC errors, allows partial success in streams

## Future Improvements

### Enum Format Requirements

For synapse IR Builder to correctly parse enum types, Rust enums must use **internally-tagged** format:

```rust
// ✅ CORRECT - internally tagged
#[derive(Serialize, Deserialize, JsonSchema)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum MyEnum {
    VariantA { field: String },
    VariantB { other: i32 },
}
// Wire format: {"type": "variant_a", "field": "..."}

// ❌ WRONG - adjacently tagged (serde default)
#[derive(Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum MyEnum {
    VariantA { field: String },
}
// Wire format: {"variant_a": {"field": "..."}}
```

**Why this matters:** The internally-tagged format puts the discriminant inline with the data, making it possible to parse the enum without prior knowledge of all variants. Adjacently-tagged format wraps the variant name as a key around the data, which is incompatible with synapse's IR Builder parser.

**Future:** plexus-macros should enforce this at compile time or emit warnings for enums without `#[serde(tag = "type")]`.

### CLI Parameter Validation

**Problem**: When required parameters are missing, synapse returns a generic "Internal error" (-32603) instead of a helpful message like "missing required parameter: count".

**Solution approach**:
1. Synapse already fetches schema via `plexus.schema`
2. Before invoking `plexus.call`, validate provided params against the schema's `required` array
3. If missing required params, emit a clear error: `Error: missing required parameter(s): count`
4. This validation happens client-side before the RPC call

---

## Implementation Notes: Codegen Pipeline Fixes (Completed)

### 1. Plexus-macros $defs Extraction (commit `cdd9ebf`)

**Problem:** Method parameter schemas were extracted from the root schema without including their `$defs` section. This left type references unresolved at the IR level.

For example, a method using `ConeIdentifier` would receive:
```json
{
  "params": {
    "properties": {
      "identifier": { "$ref": "#/$defs/ConeIdentifier" }  // 🔴 dangling reference
    }
  }
}
```

**Root cause:** The `method_enum.rs` codegen only extracted the relevant `properties` but not the supporting `$defs` that defined custom types.

**Solution:** Modified `/Users/user/dev/controlflow/hypermemetic/plexus-macros/src/codegen/method_enum.rs` to:
1. Extract the method's param schema as before
2. Look up the root schema's `$defs` section
3. Merge all definitions into the method's schema
4. Result: self-contained schema with all type definitions available

```json
{
  "params": {
    "properties": {
      "identifier": { "$ref": "#/$defs/ConeIdentifier" }
    },
    "$defs": {
      "ConeIdentifier": {
        "oneOf": [
          { "type": "object", "properties": { "by_name": { "type": "string" } } },
          { "type": "object", "properties": { "by_uuid": { "type": "string" } } }
        ]
      }
    }  // ✅ now resolved
  }
}
```

### 2. ConeIdentifier Enum Format (commit `a563b8d`)

**Problem:** The `ConeIdentifier` enum was serialized in adjacently-tagged format, which synapse's IR Builder couldn't parse:

```json
{
  "by_name": {
    "name": "my-cone"
  }
}
```

Synapse IR Builder expects the discriminant to be inline with the data (internally-tagged format), not wrapping it as a key.

**Solution:** Updated `/Users/user/dev/controlflow/hypermemetic/substrate/src/activations/cone/methods.rs` to use internally-tagged format:

```rust
#[derive(Serialize, Deserialize, JsonSchema)]
#[serde(tag = "type", rename_all = "snake_case")]  // ✅ added tag = "type"
pub enum ConeIdentifier {
    ByName { name: String },
    ByUuid { uuid: String },
}
```

Now serializes as:
```json
{
  "type": "by_name",
  "name": "my-cone"
}
```

**Impact:** All enums in the codebase should use this format. This is now documented in the "Enum Format Requirements" section above.

### Current Pipeline Status

The end-to-end codegen pipeline now works:

```
Rust schema (JSON Schema)
    ↓
synapse plexus -i
    ↓
Synapse IR (types, methods)
    ↓
service-codegen
    ↓
TypeScript client (type-safe)
    ↓
npx tsc --noEmit ✓ (compiles)
```

**Verification:**
- `synapse plexus -i` generates IR from schema
- `service-codegen` generates TypeScript client from IR
- `npx tsc --noEmit` compiles without errors
- Minor warning for `SchemaResult` type (health plugin) - harmless

**Next steps:**
- Sync these fixes to plexus-core, substrate-protocol, and synapse repositories
- Update any other enums in the codebase to use internally-tagged format
- Document the enum tagging requirement in contributing guidelines

---

## Migration Path

### Phase 1: Namespace Consistency
1. **Rename RPC methods** from `plexus_call` → `{backend}.call` (backend-namespaced dot notation)
2. **Update synapse CLI** to require explicit backend: `synapse plexus <path>`
3. **Update substrate-protocol** Transport layer for new method names

### Phase 2: Remove Double-Wrapping
4. **Add PlexusStreamItem to IR** as a core type with JSON Schema
5. **Change {backend}.call** to return `impl Stream<Item = PlexusStreamItem>`
6. **Remove CallEvent** entirely
7. **Remove synapse unwrap** - PlexusStreamItem is already the expected type

### Phase 3: Two-Layer Codegen
8. **Update service-codegen** for two-layer generation (RPC + typed wrappers)
9. **Add `call` to plexus-macros** for `parent = true` plugins

### Phase 4: Multi-Backend (Future)
10. **Backend discovery** - orchestrator exposes `backends.list`, `backends.info`
11. **Synapse multi-connect** - connect to multiple backends by namespace
12. **Direct backend access** - bypass orchestrator for performance

## Benefits

- **Uniform wire format**: PlexusStreamItem everywhere, no special cases
- **No double-wrapping**: Domain → PlexusStreamItem (once)
- **Multi-service ready**: Any service can route, same protocol
- **Clean layering**: Transport (PlexusStreamItem) vs Application (typed events)
- **Simpler synapse**: No CallEvent unwrap needed
- **Better codegen**: Clear separation of RPC layer vs typed wrappers

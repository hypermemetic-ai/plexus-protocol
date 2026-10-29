# Schema Query Refinement

## Summary

Method schemas can now be queried directly via path-based routing (`plugin.method.schema`) or parameter-based querying (`plugin.schema` with `{"method": "name"}`). Additionally, `PluginSchema` gains an optional `long_description` field with a 15-word limit enforced on the short `description`.

## Changes

### 1. New `SchemaResult` Type

```rust
// substrate/src/plexus/schema.rs
#[derive(Serialize, Deserialize)]
#[serde(untagged)]
pub enum SchemaResult {
    Plugin(PluginSchema),
    Method(MethodSchema),
}
```

The `untagged` serde attribute means the JSON output is the raw schema object - no wrapper.

### 2. Path-Based Schema Routing

```
// Plugin schemas (navigate to plugin, then call schema)
echo.schema                    → PluginSchema for echo
solar.schema                   → PluginSchema for solar (with children)
solar.earth.schema             → PluginSchema for earth (child of solar)
solar.earth.luna.schema        → PluginSchema for luna (child of earth)

// Method schemas (navigate to plugin, then method.schema)
echo.echo.schema               → MethodSchema for "echo" method
echo.once.schema               → MethodSchema for "once" method
solar.observe.schema           → MethodSchema for "observe" on solar
solar.earth.info.schema        → MethodSchema for "info" on earth
solar.jupiter.io.info.schema   → MethodSchema for "info" on io (nested)
```

**Routing logic**: When `{name}.schema` is encountered:
1. Check if `{name}` is a local method → return MethodSchema
2. Otherwise, route to child `{name}` with method `schema` → return child's PluginSchema

### 3. Parameter-Based Method Schema Query

```
// Alternative: pass method name as parameter
{backend}.call("echo.schema", {"method": "once"})  // Returns MethodSchema
{backend}.call("echo.schema", {})                   // Returns PluginSchema
```

Both approaches return identical `MethodSchema` output.

**Token savings**: A method schema is ~312 tokens vs ~728 for the full plugin.

### 4. Name Collision Detection

Runtime validation prevents ambiguous routing by detecting collisions at schema construction time:

```rust
// PluginSchema::parent() and ::leaf() will panic on:
// - Duplicate method names
// - Duplicate child names
// - Method/child name collisions

// Example: panic if plugin has both method "foo" and child "foo"
panic!("Name collision in plugin 'solar': method/child collision for 'foo'")
```

This is a **system error** - if triggered, it indicates a bug in the plugin definition that must be fixed.

### 5. Long Description Field

```rust
pub struct PluginSchema {
    pub description: String,              // max 15 words (enforced at compile time)
    pub long_description: Option<String>, // unlimited, optional
    // ...
}
```

The 15-word limit on `description` is enforced in `plexus-macros` at compile time:

```rust
// plexus-macros/src/parse.rs
const MAX_DESCRIPTION_WORDS: usize = 15;
// Compile error if exceeded
```

## Impact on Synapse

### Meaning Package Changes Required

The `meaning` Haskell package needs updates to match:

```haskell
-- meaning/src/Plexus/Schema/Recursive.hs

-- 1. Add long_description to PluginSchema
data PluginSchema = PluginSchema
  { psNamespace       :: Text
  , psVersion         :: Text
  , psDescription     :: Text
  , psLongDescription :: Maybe Text   -- NEW
  , psHash            :: PluginHash
  , psMethods         :: [MethodSchema]
  , psChildren        :: Maybe [ChildSummary]
  }

instance FromJSON PluginSchema where
  parseJSON = withObject "PluginSchema" $ \o -> PluginSchema
    <$> o .: "namespace"
    <*> o .: "version"
    <*> o .: "description"
    <*> o .:? "long_description"  -- NEW (optional)
    <*> o .: "hash"
    <*> o .:? "methods" .!= []
    <*> o .:? "children"

-- 2. Add SchemaResult type
data SchemaResult
  = SchemaPlugin PluginSchema
  | SchemaMethod MethodSchema
  deriving stock (Show, Eq)

instance FromJSON SchemaResult where
  parseJSON v =
    -- Try PluginSchema first (has "namespace" field)
    (SchemaPlugin <$> parseJSON v) <|>
    -- Fall back to MethodSchema (has "name" field)
    (SchemaMethod <$> parseJSON v)
```

### Synapse CLI Changes

Synapse already has a `SchemaView` type that distinguishes plugins from methods:

```haskell
-- synapse/src/Synapse/Schema/Types.hs
data SchemaView
  = ViewPlugin PluginSchema Path
  | ViewMethod MethodSchema Path
```

**Recommended enhancement** for `--schema` flag:

```haskell
-- Current: synapse --schema solar       -> fetches solar.schema
-- Current: synapse --schema solar earth -> fetches solar.schema, then solar.earth.schema

-- NEW: when path ends at a method, use method-specific query
-- synapse --schema echo once
--   -> fetches echo.schema with {"method": "once"}
--   -> returns just MethodSchema (smaller response)
```

Implementation in `Synapse/Algebra/Navigate.hs`:

```haskell
-- When navigating to a method, can now fetch just that method's schema
fetchMethodSchema :: Text -> Text -> SynapseM MethodSchema
fetchMethodSchema plugin method = do
  result <- rpcCall (plugin <> ".schema") (object ["method" .= method])
  case result of
    SchemaMethod m -> pure m
    SchemaPlugin _ -> throwError "Expected method schema"
```

### Backward Compatibility

1. **Wire format**: Fully backward compatible
   - `long_description` is optional (skipped if None)
   - Schema endpoint without params returns same format as before
   - `SchemaResult` is untagged, so old clients parsing `PluginSchema` still work

2. **Synapse**: Will work unchanged but won't get `long_description` until updated

3. **MCP**: Tools list unchanged; method-specific schema is opt-in

## Migration Path

1. **meaning**: Add `psLongDescription` field with `.:?` (optional parsing)
2. **synapse**: Add `SchemaResult` parsing, update `--schema` to use method param when applicable
3. **substrate**: Already complete

## Files Changed

### Substrate
- `src/plexus/schema.rs` - Added `SchemaResult`, `long_description` field
- `src/plexus/mod.rs` - Export `SchemaResult`
- `src/plexus/plexus.rs` - Added `long_description()` to Activation trait
- `src/activations/health/activation.rs` - Updated schema handling
- `src/activations/solar/celestial.rs` - Updated schema handling

### Plexus-macros
- `src/parse.rs` - Added `long_description` parsing, 15-word validation
- `src/codegen/mod.rs` - Pass through long_description
- `src/codegen/activation.rs` - Generate long_description(), updated schema dispatch

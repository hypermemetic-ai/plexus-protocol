# Changelog for plexus-protocol

## 0.8.0.0

A schema's slots as plexus-core 0.10 writes them (NEST-5, NEST-7;
SYNAPSE-41). Breaking: `MethodRole` gains a constructor and a field, so an
exhaustive match or a positional pattern on it changes.

### Breaking

- **`MethodRolePeer { listMethod, searchMethod, peerInterface }`**: a peer
  slot (`#[plexus_macros::peer]`, e.g. plexus-dyn's `dyn.peer.<name>`). On the
  wire it is plexus-core's `{"kind": "static_child", "peer": true, …}` — read
  as `MethodRolePeer` and written back the same; a `static_child` without
  `peer` (or with `peer: false`) is `MethodRoleStaticChild` as before.
- **`MethodRoleDynamicChild` gains `childSchema :: Maybe PluginSchema`**: a
  typed dynamic child's slot type (`child`). `MethodRoleDynamicChild lm sm`
  is now `MethodRoleDynamicChild lm sm ch`.

### Added

- `PeerInterface` (`name`, `hash`, `ir` kept as JSON, `methods`, `schema`):
  plexus-core's `peer::Interface`. A slot type or interface this reader
  cannot read leaves the slot untyped rather than failing the schema.
- `isSlotRole`: a dynamic child or a peer slot — a role whose names are open.
- `Plexus.Transport.fetchSchemaAtIfAny`: the schema at a path that may not be
  there — `Right Nothing` when the service says activation or method not
  found (or a slot's `not_found`), every other failure `Left` as `fetchSchemaAt` says it;
  `schemaNotFound` is its test on a call's items.

## 0.7.0.0

bidir removed (ASK-1, ASK-3). Breaking.

### Breaking

- **bidir is gone.** The server no longer asks its caller mid-call: the
  `request` stream item (`StreamRequest`), `Request` / `Response`
  (`Confirm`, `Prompt`, `Select`; `Confirmed`, `Value`, `Selected`,
  `Cancelled`), `SelectOption`, `WellKnownRequest` / `WellKnownResponse`,
  `StandardRequest` / `StandardResponse` and `sendBidirectionalResponse`
  (`{ns}.respond`) are removed. A `request` item is no stream item.
- **`MethodSchema`**: `methodBidirectional`, `methodRequestType` and
  `methodResponseType` are removed, with nothing in their place. An older
  schema's `bidirectional` (and its request / response types) is read past.
- Plexus has no in-call or protocol-wide confirmation: a service that wants
  one builds it as its own methods. The input half (BIDIR-18,
  `Plexus.Input`) stays.

## 0.6.0.0

The Haskell side of plexus 0.7's schema and envelope. Breaking.

### Breaking

- **Method schemas (`Plexus.Schema.Recursive`).** `MethodSchema` gains
  `methodReturns` (required), `methodError` and `methodProgress`, and
  `methodCardinality :: Cardinality` (`CardinalityOne` / `CardinalityMany`).
  `methodStreaming` is removed. A schema from the published 0.7.0 line (no
  `cardinality`) is still read, its cardinality taken from the method's own
  `streaming` flag; cardinality is never guessed otherwise.
  `psSelfHash` (`self_hash`) and `methodHttpMethod` (`http_method`, default
  `"POST"`) are read. Absent optional fields encode as omitted, not `null`,
  as the Rust side writes them.
- **Typed remote errors (new `Plexus.RemoteError`).** `RemoteError` is one
  of `ActivationNotFound`, `MethodNotFound`, `InvalidParams`,
  `Unauthenticated`, `Forbidden DenyReason` (including `MissingGrant`),
  `HandleNotSupported`, `Execution`, or an open variant that keeps an
  unknown kind and all its fields and writes them back as they came.
  `renderRemoteError` gives the Rust `Display` line.
- **Stream items (`Plexus.Types`).** `StreamError` carries
  `error :: RemoteError` and `recoverable` (was `message`), keeping the
  server's own message and any refusal fields sent beside it.
  `StreamProgress` carries `content_type` and `content` (was `message`,
  `percentage`); a 0.7.0 progress frame is read as a note.
  `RpcError.errData` is `Maybe RemoteError`; `RpcError` has a `ToJSON`
  instance. `-32601` is typed as an activation or method not found by its
  message.
- `Plexus.Client` yields a rejection's `RemoteError` as its `StreamError`;
  the `Transport` and `Schema` helpers render it to `Text` where they used
  to return the message.

### Added

- **The input half of a call (new `Plexus.Input`, plexus-wire spec/10).**
  The frames a client sends into a call (`<ns>.input`, `<ns>.end` and the
  binary frame), the signals the callee sends back (`credit`, `ack`,
  `input_closed`) and `itemKind`. `Plexus.Client.substrateRpcDuplex` hands a
  call its `InputPort`; credit, ack and input_closed are taken off before
  item parsing. `substrateRpcSub` is unchanged for a method without input.
  `Plexus.Transport` adds `DuplexRoute`, `setDuplexRouter` and
  `invokeMethodDuplex`.
- Test suite `plexus-protocol-test`: the plexus 0.7 conformance fixture
  parsed and re-encoded.

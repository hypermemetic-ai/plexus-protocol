# Changelog for plexus-protocol

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

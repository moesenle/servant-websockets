# Introduction

This small library provides two servant endpoints for implementing
websockets and is based on `websockets` and `wai-websockets`.

This library provides two `servant` endpoints: `WebSocket` and
`WebSocketConduit`. The former is a low-level interface for directly
interacting with a `Connection` (see the
[websockets](https://hackage.haskell.org/package/websockets) library
for more information). The latter provides a
[conduit](https://hackage.haskell.org/package/conduit) based endpoint
for JSON serializable input and output.

See the module documentation for examples.

# Breeze — Feature Documentation

> A high-performance HTTP framework for Go, built on top of [gnet](https://github.com/panjf2000/gnet) (an event-driven, non-blocking networking library). Breeze is designed around zero-allocation hot paths, a composable middleware chain, and a batteries-included middleware suite.

---

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Core Server (`Breeze`)](#core-server-breeze)
3. [Router](#router)
4. [Context](#context)
5. [HTTP Request Parsing](#http-request-parsing)
6. [HTTP Response Serialization](#http-response-serialization)
7. [Worker Pool](#worker-pool)
8. [File & Multipart Upload Handling](#file--multipart-upload-handling)
9. [Middlewares](#middlewares)
   - [Logger](#logger)
   - [Panic Recovery](#panic-recovery)
   - [CORS](#cors)
   - [Security Headers (Helmet)](#security-headers-helmet)
   - [JWT Authentication](#jwt-authentication)
   - [Rate Limiter](#rate-limiter)
   - [ETag Cache](#etag-cache)
   - [Compression](#compression)
   - [Swagger / OpenAPI](#swagger--openapi)
10. [Static File Serving](#static-file-serving)
11. [Performance Design Decisions](#performance-design-decisions)
12. [Quick Start Example](#quick-start-example)

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────┐
│                   gnet event loop                   │
│  (non-blocking, multicore, round-robin balancing)   │
└────────────────────┬────────────────────────────────┘
                     │ raw TCP bytes
                     ▼
            ┌────────────────┐
            │  ParseHTTPRequest  │  zero-alloc scanner
            └───────┬────────┘
                    │ *HTTPRequest
                    ▼
            ┌────────────────┐
            │    Router.Find │  pre-computed segment matching
            └───────┬────────┘
                    │ handler + middlewares + params
                    ▼
            ┌────────────────┐
            │  WorkerPool /  │  off-loads from event loop
            │  goroutine     │
            └───────┬────────┘
                    │ Context.Next() chain
                    ▼
            ┌────────────────┐
            │  Middlewares → │  CORS, JWT, Rate-limit, etc.
            │  Handler       │
            └───────┬────────┘
                    │ *HTTPResponse
                    ▼
            ┌────────────────┐
            │  Response.Bytes│  strconv, no fmt.Sprintf
            └───────┬────────┘
                    │ raw bytes
                    ▼
             c.AsyncWrite()
```

---

## Core Server (`Breeze`)

**File:** `breeze.go`

`Breeze` is the main server struct. It embeds `gnet.BuiltinEventEngine` and wires raw TCP traffic into the HTTP pipeline.

### Key Behaviours

| Feature | Detail |
|---|---|
| Per-connection buffering | Uses `sync.Map` keyed by file descriptor (`fd`) to store partial request bytes. Eliminates a single global mutex bottleneck under multicore mode. |
| Buffer compaction | After consuming a request, leftover bytes smaller than `compactThreshold` (512 bytes) are copied to a fresh slice so the large backing receive buffer can be GC'd. |
| Pipelined requests | The `OnTraffic` loop processes multiple HTTP requests from a single read call. |
| 400 Bad Request | Returned inline (no handler dispatch) when `ParseHTTPRequest` returns an error. |
| 404 Not Found | Returned inline when `Router.Find` returns no handler. |
| Connection cleanup | `OnClose` deletes the per-connection buffer map entry on disconnect. |

### Starting the Server

```go
app := breeze.New(router, pool)
app.Run(3000, true)  // port, multiCore
```

Options passed to gnet:
- `TCPNoDelay` — reduces latency for small messages.
- `Multicore` — spawns one event-loop goroutine per CPU core.
- `RoundRobin` — distributes connections evenly across loops.

---

## Router

**File:** `router.go`, `router_static.go`

### Route Registration

```go
router := breeze.NewRouter()
router.Handle(breeze.GET, "/users/:id", handlerFn, optionalMiddleware...)
```

Supported HTTP methods (defined in `types.go`): `GET`, `POST`, `PUT`, `PATCH`, `DELETE`, `OPTION`.

### Path Matching

Routes are matched segment-by-segment with early bail-out on count mismatch:

- **Static segments** — exact string comparison (`/users`, `/health`).
- **Named parameters** — `:param` captures a single path segment (e.g. `/users/:id`).
- **Wildcard** — `*name` at the end of a pattern captures zero or more remaining segments (e.g. `/files/*filepath`).

Path parameter index (`paramIndex[]`) and count (`paramCount`) are pre-computed at registration, so the hot match loop avoids `strings.HasPrefix` calls and only allocates a `map[string]string` when the route actually has params.

### Global Middleware

```go
router.Use(middleware.LoggingMiddleware())
router.Use(middleware.CORSMiddleware(opts))
```

Global middlewares run before every route handler.

### Per-Route Middleware

```go
router.Handle(breeze.POST, "/upload", handler, authMiddleware, rateLimitMiddleware)
```

Per-route middlewares are captured at registration into an immutable slice (copied so callers cannot mutate them later).

### Auto-Serve Root

When `autoServeRoot` is `true` (default), a `GET /` request automatically serves `./public/index.html` if the file exists, without requiring an explicit route.

---

## Context

**File:** `context.go`

`Context` is passed to every handler and middleware. It carries the connection, parsed request, response, route params, and the middleware chain index.

### Response Helpers

| Method | Content-Type | Status |
|---|---|---|
| `ctx.JSON(v any)` | `application/json` | 200 |
| `ctx.WriteString(s)` | `text/plain` | 200 |
| `ctx.HTML(data []byte)` | `text/html; charset=utf-8` | 200 |
| `ctx.Status(code int)` | — | custom |
| `ctx.SetHeader(key, value)` | — | — |

`JSON`, `WriteString`, and `HTML` reuse shared package-level header maps for zero allocation. `SetHeader` performs **copy-on-write** — the first mutation upgrades the shared map to a private copy so the package-level maps are never clobbered.

### Parameter Helpers

```go
ctx.Param("id")          // read a :param value
ctx.GetParam("id")       // alias
ctx.Query("page")        // read a query string value (?page=2)
ctx.SetParam("key", "v") // write a param (useful in middleware)
ctx.GetParams()          // returns a copy of all params
```

### Middleware Chain

```go
ctx.Next()   // advance to the next middleware/handler
ctx.Abort()  // short-circuit — skip all remaining handlers
```

---

## HTTP Request Parsing

**File:** `request.go`

`ParseHTTPRequest(data []byte)` is a from-scratch HTTP/1.1 parser that returns `(*HTTPRequest, bytesConsumed, error)`.

### Performance Techniques

| Technique | Why |
|---|---|
| `unsafe.String` (`b2s`) | Converts `[]byte → string` without allocation. Safe because the byte slice lives in the per-connection buffer for the full request lifetime. |
| Manual request-line scan | Uses `bytes.IndexByte` instead of `bytes.Split`, avoiding a `[][]byte` allocation. |
| `toLowerASCII` fast path | Only allocates a lowercase buffer when a header key actually contains uppercase characters. |
| `splitPathQuery` | Splits path/query at `?` using `bytes.IndexByte` — no allocation, no `net/url` overhead for the common no-query case. |
| Pre-sized header map | `make(map[string]string, 8)` avoids rehash for the typical 4–8 header case. |
| Incremental body | Returns `nil, 0, nil` when `Content-Length` bytes haven't arrived yet; caller retries on next read. |

---

## HTTP Response Serialization

**File:** `response.go`

`(*HTTPResponse).Bytes()` serializes a response to raw HTTP/1.1 wire bytes.

### Performance Techniques

- **No `fmt.Sprintf`** — status code and `Content-Length` are written with `strconv.AppendInt` directly into the buffer.
- **Pre-sized buffer** — estimated capacity `32 + len(status) + headers×48 + body` avoids growth reallocations for typical responses.
- **Array-indexed status text** — `statusTexts[code]` is an O(1) array lookup (a `[600]string`), not a map hash.

---

## Worker Pool

**File:** `workerpool.go`

```go
pool := breeze.NewWorkerPool(runtime.NumCPU())
```

A fixed goroutine pool with a buffered task channel (`concurrency × 16`) absorbs request bursts without blocking gnet's event loop.

### Behaviour

| Scenario | Behaviour |
|---|---|
| Queue has capacity | Task is enqueued normally. |
| Queue is full (burst) | Falls back to `go task()` — never blocks the event-loop goroutine. |
| Graceful shutdown | `pool.Shutdown(ctx)` waits for all in-flight tasks to complete, or until the context expires. |

When `pool` is `nil`, the server falls back to `go exec()` per request.

---

## File & Multipart Upload Handling

**File:** `file.go`

### Parse Multipart Form

```go
files, fields, err := ctx.ParseMultipart(10 << 20) // 10 MB per file limit
```

Returns:
- `files map[string][]*UploadedFile` — keyed by form field name.
- `fields map[string][]string` — non-file form fields.

`UploadedFile` carries: `Field`, `Filename`, `Header` (MIME headers), `ContentType` (from header or auto-sniffed), `Size`, and `Content []byte`.

Content-type detection falls back to `http.DetectContentType` when the part header is absent.

### Save Uploaded File

```go
savedName, err := ctx.SaveUploadedFile("avatar", "/uploads/user-123.jpg", 5<<20)
```

Parses the multipart body, picks the first file from `fieldName`, creates the destination directory if needed, and writes the file to disk.

---

## Middlewares

All middlewares live in the `middlewares/` package and implement `breeze.HandlerFunc`.

### Logger

**File:** `middlewares/logger.go`

```go
router.Use(middleware.LoggingMiddleware())
```

Logs each request in the format:

```
[Breeze][2026-06-23T12:00:00Z] GET /users -> 200 (1.2ms)
```

Captures timing by recording `time.Now()` before calling `ctx.Next()` and measuring `time.Since` after.

---

### Panic Recovery

**File:** `middlewares/panic_recovery.go`

```go
router.Use(middleware.RecoveryMiddleware())
```

Wraps the entire handler chain in a `defer/recover`. On panic:
1. Prints the panic value and full stack trace (`debug.Stack()`).
2. Sets response status 500 with body `"Internal Server Error"`.
3. Calls `ctx.Abort()` to stop further middleware execution.

---

### CORS

**File:** `middlewares/cors.go`

```go
router.Use(middleware.CORSMiddleware(middleware.CORSOptions{
    AllowOrigins:     "*",
    AllowMethods:     "GET,POST,PUT,DELETE",
    AllowHeaders:     "Content-Type,Authorization",
    AllowCredentials: "true",
    MaxAge:           "86400",
}))
```

Sets `Access-Control-*` response headers. Handles preflight `OPTIONS` requests by returning `204 No Content` immediately without continuing the chain.

All fields are optional — only non-empty values produce headers.

---

### Security Headers (Helmet)

**File:** `middlewares/helmet.go`

```go
// Opinionated safe defaults
router.Use(middleware.DefaultSecurityMiddleware())

// Custom configuration
router.Use(middleware.SecurityMiddleware(middleware.SecurityOptions{
    ContentSecurityPolicy:   "default-src 'self'",
    XFrameOptions:           "SAMEORIGIN",
    StrictTransportSecurity: "max-age=31536000",
}))
```

Supported headers:

| Header | Default |
|---|---|
| `Content-Security-Policy` | `default-src 'self'` |
| `X-Frame-Options` | `DENY` |
| `X-Content-Type-Options` | `nosniff` |
| `Referrer-Policy` | `no-referrer` |
| `Strict-Transport-Security` | `max-age=63072000; includeSubDomains; preload` |
| `Permissions-Policy` | `geolocation 'none'; microphone 'none'; camera 'none'` |
| `X-XSS-Protection` | `1; mode=block` |
| `Expect-CT` | `max-age=86400, enforce` |
| `Cross-Origin-Embedder-Policy` | `require-corp` |
| `Cross-Origin-Opener-Policy` | `same-origin` |
| `Cross-Origin-Resource-Policy` | `same-origin` |
| `Cache-Control` | `no-store, no-cache, must-revalidate` |

Convenience constructors: `WithContentSecurityPolicy`, `WithXFrameOptions`, `WithReferrerPolicy`.

---

### JWT Authentication

**File:** `middlewares/jwt.go`

```go
router.Use(middleware.JWTAuthMiddleware(middleware.JWTOptions{
    AccessSecret:       "my-access-secret",
    RefreshSecret:      "my-refresh-secret",
    SigningMethod:      jwt.SigningMethodHS256,
    RequiredRoles:      []string{"admin"},
    EnableRefreshToken: true,
    ClaimsValidator: func(claims jwt.MapClaims) bool {
        return claims["active"] == true
    },
}))
```

#### Features

- **Token extraction** — defaults to `Authorization: Bearer <token>`. Override via `TokenLookup func(*Context) (accessToken, refreshToken, error)`.
- **Refresh token support** — when `EnableRefreshToken: true` and the access token is expired, the middleware validates the refresh token, issues a new access token, and returns it in the `X-New-Access-Token` response header.
- **Role-based access control** — `RequiredRoles` checks the `role` claim; returns 401 if no match.
- **Custom claims validation** — `ClaimsValidator` runs arbitrary logic against the parsed `jwt.MapClaims`.
- **Claims in context** — validated claims are stored as `ctx.SetParam(UserContextKey, ...)` for use downstream.
- **Custom 401 handler** — override `OnUnauthorized` to return custom error shapes.

#### Helper Functions

```go
// Generate tokens
token, err := middleware.GenerateJWT(secret, jwt.MapClaims{"user_id": "123"}, 15*time.Minute, nil)
refresh, err := middleware.GenerateRefreshToken(secret, claims, 7*24*time.Hour, nil)
```

---

### Rate Limiter

**File:** `middlewares/rate_limiter.go`

```go
router.Use(middleware.NewRateLimiter(middleware.RateLimiterOptions{
    Requests: 100,
    Per:      time.Minute,
    Message:  "Slow down!",
}))
```

Tracks request counts per remote IP address using a `sync.Mutex`-protected in-memory map. Resets the counter when the `Per` duration elapses. Returns `429 Too Many Requests` when the limit is exceeded.

---

### ETag Cache

**File:** `middlewares/cache.go`

```go
cache := middleware.NewETagCache()
router.Use(cache.ETagMiddleware())
```

After a handler sets a response body, the middleware:
1. Computes an MD5 hash of the body.
2. Sets the `ETag` response header.
3. Stores the body + ETag in an in-memory map keyed by request path.
4. Checks the `If-None-Match` request header — if it matches, returns `304 Not Modified` with an empty body.

---

### Compression

**File:** `middlewares/compression.go`

```go
router.Use(middleware.CompressionMiddleware())
```

Inspects the `Accept-Encoding` request header and compresses the response body with the best supported algorithm (priority: `br` > `gzip` > `deflate`). Sets the corresponding `Content-Encoding` response header. Falls through silently if no supported encoding is advertised or if compression fails.

| Encoding | Library |
|---|---|
| Brotli (`br`) | `github.com/andybalholm/brotli` |
| Gzip | `compress/gzip` (stdlib) |
| Deflate | `compress/flate` (stdlib) |

---

### Swagger / OpenAPI

**Files:** `middlewares/swagger.go`, `swagger/`

Breeze includes a full OpenAPI 3.1 documentation system that introspects Go structs via reflection at startup — no code generation step required.

#### Enabling Swagger

```go
router.Use(middleware.SwaggerMiddleware(router, middleware.SwaggerOptions{
    Title:       "My API",
    Version:     "1.0.0",
    Description: "Optional long description.",
    JSONPath:    "/swagger.json",  // raw OpenAPI JSON
    UIPath:      "/swagger",       // Swagger UI (HTML)
}))
```

This registers two endpoints and activates doc collection. The middleware itself is a transparent pass-through at request time — all real work happens at startup.

#### Documenting Routes

```go
router.Handle(breeze.POST, "/users", createUser,
    middleware.DocPOST("/users", swagger.RouteDoc{
        Title:       "Create user",
        Tags:        []string{"Users"},
        Description: "Creates a new user account.",
        Input: []swagger.InputGroup{
            {
                Type:     swagger.InputBody,
                Fields:   CreateUserRequest{},
                Required: true,
            },
        },
        Output:       UserResponse{},
        OutputStatus: 201,
    }),
)
```

#### Input Types

| Constant | Source | OpenAPI `in` |
|---|---|---|
| `swagger.InputBody` | JSON request body | `requestBody` |
| `swagger.InputQuery` | URL query string | `query` |
| `swagger.InputParams` | Path parameters | `path` |
| `swagger.InputHeader` | Request headers | `header` |

#### Schema Inference

`swagger.InferSchema(v any)` uses `reflect` to derive a full OpenAPI `Schema` from any Go value:

- Structs → `object` with `properties` and `required` list (fields without `omitempty` are required).
- Slices/Arrays → `array` with inferred `items` schema.
- Maps → `object` (no fixed properties).
- Primitives → `string`, `integer` (with `int32`/`int64` format), `number` (`float`/`double`), `boolean`.
- Struct field tags `description:"..."` and `example:"..."` are reflected into the schema.
- JSON field name comes from the `json:"..."` tag; fields tagged `json:"-"` are excluded.

#### Convenience Helpers

```go
// Per-method doc helpers
middleware.DocGET(path, doc)
middleware.DocPOST(path, doc)
middleware.DocPUT(path, doc)
middleware.DocPATCH(path, doc)
middleware.DocDELETE(path, doc)

// Tag helper
middleware.Tag("Users", swagger.RouteDoc{...})
```

---

## Static File Serving

**File:** `router_static.go`

```go
router.ServeStatic("/static", "./public")
```

Registers a wildcard route `GET /static/*filepath` that:
1. Resolves the request path relative to `root`.
2. Sanitizes against directory traversal (`filepath.Clean`).
3. Detects content type from file extension (`mime.TypeByExtension`), falling back to `http.DetectContentType`.
4. Returns `404` for missing files and directories.

Auto-serve root (`GET /`) serves `./public/index.html` without needing an explicit `ServeStatic` call.

---

## Performance Design Decisions

| Decision | Rationale |
|---|---|
| `gnet` event loop | Non-blocking I/O avoids per-connection goroutine overhead at massive concurrency. |
| `sync.Map` for buffers | Replaces `map + Mutex`, eliminating cross-reactor mutex contention in multicore mode. |
| Buffer compaction (`compactThreshold`) | Prevents a large 64KB receive buffer from being pinned alive by a 10-byte leftover slice. |
| Stack-allocated segment array (`[16]string`) | Avoids heap allocation for path matching on routes with ≤ 16 segments. |
| Pre-computed `paramIndex[]` / `paramCount` | Eliminates `strings.HasPrefix` and conditional map allocation from the hot-path match loop. |
| Shared header maps + copy-on-write | `JSON`/`WriteString`/`HTML` never allocate a new header map; `SetHeader` upgrades lazily. |
| `unsafe.String` zero-copy parse | Removes the `[]byte → string` copy during request line and header parsing. |
| `strconv.AppendInt` in response serializer | Avoids `fmt.Sprintf` formatting overhead for status code and content-length. |
| Array-indexed status text | O(1) status-to-text lookup versus map hash. |
| Worker pool channel `× 16` | Absorbs burst traffic without blocking the event-loop goroutine. |
| `go-json` for JSON | Faster JSON marshal than `encoding/json` from stdlib. |

---

## Quick Start Example

```go
package main

import (
    "runtime"
    "time"

    "github.com/nelthaarion/breeze"
    middleware "github.com/nelthaarion/breeze/middlewares"
)

func main() {
    router := breeze.NewRouter()

    // Global middleware stack
    router.Use(middleware.RecoveryMiddleware())
    router.Use(middleware.LoggingMiddleware())
    router.Use(middleware.CORSMiddleware(middleware.CORSOptions{
        AllowOrigins: "*",
        AllowMethods: "GET,POST,DELETE",
        AllowHeaders: "Content-Type,Authorization",
    }))
    router.Use(middleware.DefaultSecurityMiddleware())

    // Rate limiting: 60 requests per minute per IP
    router.Use(middleware.NewRateLimiter(middleware.RateLimiterOptions{
        Requests: 60,
        Per:      time.Minute,
    }))

    // Protected route group
    protected := middleware.JWTAuthMiddleware(middleware.JWTOptions{
        AccessSecret: "super-secret",
    })

    router.Handle(breeze.GET, "/", func(ctx *breeze.Context) {
        ctx.WriteString("Hello from Breeze!")
    })

    router.Handle(breeze.GET, "/users/:id", getUser, protected)

    router.Handle(breeze.POST, "/upload", func(ctx *breeze.Context) {
        name, err := ctx.SaveUploadedFile("file", "./uploads/file.bin", 10<<20)
        if err != nil {
            ctx.Status(400)
            ctx.WriteString(err.Error())
            return
        }
        ctx.JSON(map[string]string{"saved": name})
    }, protected)

    // Static assets
    router.ServeStatic("/assets", "./public")

    app := breeze.New(router, breeze.NewWorkerPool(runtime.NumCPU()))
    app.Run(3000, true)
}

func getUser(ctx *breeze.Context) {
    ctx.JSON(map[string]string{
        "id":   ctx.Param("id"),
        "name": "Alice",
    })
}
```

---

## FastAPI vs FastAPI-StartKit (Python benchmark)

Two Python ASGI entries in this harness implement the **same three benchmark
routes** with identical handlers (`GET /` → empty, `GET /user/{id}` → the id as
plain text, `POST /user` → empty). This section compares them.

### Overview

| | **fastapi** | **fastapi-startkit** |
|---|---|---|
| What it is | The raw [FastAPI](https://fastapi.tiangolo.com) micro-framework (Starlette + Pydantic). | A batteries-included application framework built **on top of** FastAPI (Laravel-style providers / config / service container). |
| Benchmark app | A single `FastAPI()` instance in `server.py` with three route handlers. | An `Application` booted with a `FastAPIProvider`; the same three routes are registered through the framework's router (`providers/`, `config/`, `bootstrap/`). |
| Directory | `python/fastapi` | `python/fastapi_startkit` |

Both wrap the same underlying FastAPI / Starlette request path — `fastapi-startkit`
adds a thin framework layer around it.

### Correctness

Both pass the shared route spec (`.spec/route_spec.rb`) **6/6**, verified through
the standard Docker + rspec harness and confirmed in-container
(`docker exec … curl localhost:3000`, so it is immune to host-networking
artifacts):

| Route | Expected | fastapi | fastapi-startkit |
|---|---|---|---|
| `GET /` | `200`, empty body | ✅ | ✅ |
| `GET /user/0` | `200`, body `0` | ✅ | ✅ |
| `POST /user` | `200`, empty body | ✅ | ✅ |

### Setup / dependencies

| | **fastapi** | **fastapi-startkit** |
|---|---|---|
| Declared dependency | `fastapi>=0.139,<0.140` | `fastapi-startkit[fastapi]==0.47.0` |
| Underlying FastAPI | `0.139.0` | `0.139.0` (transitive, via `fastapi-startkit==0.47.0`) |
| App composition | single `FastAPI()` in `server.py` | `Application` + `FastAPIProvider` (providers / config / bootstrap) |
| Engines | uvicorn (default), hypercorn, daphne, granian | uvicorn (default), hypercorn, daphne, granian |
| Default build | `.Dockerfile.uvicorn` | `.Dockerfile.uvicorn` |
| Python | 3.14 | 3.14 |

The `fastapi-startkit` dependency is pinned to `==0.47.0`, which resolves the
**same FastAPI 0.139.0 / Starlette 1.3.1** the baseline uses, so both Python
entries benchmark on an identical FastAPI/Starlette stack; both are built and run
from the same default `.Dockerfile.uvicorn` engine.

### Performance

Both serve identical handlers over the **same ASGI server (uvicorn)** in the
**same container recipe**, so the per-request hot path is the same FastAPI /
Starlette routing; `fastapi-startkit` adds framework wiring at **boot** (provider
/ router / container setup), not on the per-request path.

A controlled, same-host relative comparison (from the Benchmark Runner) shows the
two are **close on the GET routes** (within a few percent) with a **larger
framework overhead on `POST /user`** at higher concurrency.

**Relative, same-host comparison — NOT official benchmark figures.**

Method (verbatim):

> macOS + OrbStack; Docker python:3.14-slim; uvicorn --workers=nproc (11
> workers); load = oha 1.14, keepalive/connection-reuse, latency-correction;
> 8s × 4 reps averaged; concurrency 64 and 256; 100% success every cell. Matched
> stack both sides: Python 3.14.6, fastapi 0.139.0, starlette 1.3.1
> (fastapi_startkit built from fastapi-startkit 0.47.0).

Relative delta (fastapi-startkit vs fastapi; negative = fastapi-startkit slower):

| Route | Δ @ c64 | Δ @ c256 |
|---|---|---|
| `GET /` | −2.9% | −6.4% |
| `GET /user/0` | −1.5% | −6.2% |
| `POST /user` | −17.3% | −18.8% |

Absolute throughput (host-specific, for context only): `fastapi` ≈ 41–47k rps,
`fastapi-startkit` ≈ 33–40k rps, 100% success across all cells.

Disclaimer (verbatim):

> Relative, same-host comparison (macOS/OrbStack), NOT official benchmark
> figures. Load uses keepalive, not the harness's --disable-keepalive;
> authoritative numbers require run.sh on Linux. Requires fastapi-startkit 0.47
> so both run FastAPI 0.139.

> ✅ **Both sides match the committed build.** The committed benchmark pins
> **`fastapi-startkit==0.47.0`**, which resolves the same **FastAPI 0.139.0 /
> Starlette 1.3.1** the baseline uses. These deltas therefore reflect the
> committed, apples-to-apples build — not a version-skewed comparison.
>
> **Why `POST /user` is slower.** It is *not* `fastapi-startkit` per-request
> code — the hot path is stock FastAPI / Starlette. The gap comes from
> **FastAPI 0.139's `include_router`**: unlike FastAPI ≤ 0.124 (which flattened
> included routes into the app router), 0.139 keeps a nested
> `fastapi.routing._IncludedRouter` node on `app.router.routes`, adding a
> per-request resolution layer. This is **intended FastAPI behavior** (cached
> candidate resolution / router-identity), and it is **reproducible in plain
> FastAPI** — any app that registers routes via `include_router` on 0.139 pays
> it. `fastapi-startkit` uses `include_router` as its idiomatic route-module
> registration, so the benchmark shows this representative cost rather than
> hiding it (e.g. via flat `add_api_route`).
>
> `include_router`'s overhead is a long-standing, acknowledged FastAPI topic.
> A related upstream discussion,
> [fastapi/fastapi#5343](https://github.com/fastapi/fastapi/issues/5343),
> reports `include_router` performance cost and requests a top-level
> `FastAPI(router=…)` bypass — though that report concerns `include_router`'s
> *construction-time* cost (per-route introspection in the app-factory pattern),
> a different mechanism from the *per-request* `_IncludedRouter` resolution
> measured here. Both reflect the same theme: `include_router` carries overhead
> that flat registration avoids.

> ⚠️ **Benchmarking caveat.** Reliable *absolute* numbers under the harness's
> standard `--disable-keepalive` were not obtainable on macOS (ephemeral-port /
> `TIME_WAIT` exhaustion), so the figures above use keepalive and are host-
> specific. Authoritative absolute throughput/latency requires the Linux
> harness (`run.sh`).
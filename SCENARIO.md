# Benchmark scenario

Every implementation must pass this HTTP contract before results for this
scenario are compared. Requests go to port `3000`. A successful response has status
`200`; an empty response has a zero-length body. JSON responses use
`Content-Type: application/json` and are compared as parsed JSON, so object key
order and whitespace do not matter. SHA-256 digests are lowercase hexadecimal.

The route is spelled `/heath` in this scenario. The existing `GET /` route remains
in use by the current benchmark runner during migration.

| # | Request | Purpose | Response |
|---|---|---|---|
| 1 | `GET /heath` | Static routing | Empty body |
| 2 | `GET /user/{user}` | Dynamic path routing | The decoded `user` value as UTF-8 plain text |
| 3 | `POST /user` | Compatibility with the existing benchmark | Empty body |
| 4 | `GET /serialization?n={n}&seed={seed}` | JSON serialization | An `items` array of exactly `n` objects derived from `seed` |
| 5 | `POST /deserialization` | JSON deserialization | Item count and SHA-256 checksum of the `value` fields |
| 6 | `POST /upload` | Multipart parsing and file hashing | SHA-256 checksum of the uploaded file bytes |

## Exact requests and responses

### 1. Static route

`GET /heath` returns `200` with an empty body.

### 2. Dynamic route

`GET /user/{user}` returns `200` with the decoded path segment as plain text. Test
with at least two distinct values, including a nonnumeric value such as
`/user/alice`; `/user/42` must return `42`.

### 3. Legacy POST route

`POST /user` with an empty request body returns `200` with an empty body.

### 4. Serialization

The `/serialization` route requires a non-null `n` query parameter that parses
as a non-negative base-10 integer from `0` through `1000`, inclusive. `seed`
must be a non-null, nonempty simple string (a scalar value, not an array or
object). For each index `i` from `0` to `n - 1`, emit an object whose `id` is
the integer `i` and whose `value` is the decoded seed followed by `:` and the
decimal representation of `i`. Return the objects in index order with
`Content-Type: application/json`:

```http
GET /serialization?n=2&seed=demo
```

```json
{"items":[{"id":0,"value":"demo:0"},{"id":1,"value":"demo:1"}]}
```

For `n=0`, return `{"items":[]}`. Test more than one `n` and more than one
`seed` to verify that both parameters affect the response. A missing or invalid
parameter, or an `n` outside the specified range, returns `400`.

### 5. Deserialization

Send this fixed JSON body with `Content-Type: application/json`:

```json
{"items":[{"value":"alpha"},{"value":"beta"},{"value":"gamma"}]}
```

Count the objects in `items`. Compute SHA-256 over the UTF-8 bytes of their
`value` fields joined in order by a single line-feed byte (`0A`), with no
trailing line feed. For the request above, the hashed bytes are
`alpha\nbeta\ngamma`. Return:

```json
{"count":3,"checksum":"f3220283d05d1ff2ae350cfe9e0e367cb5aef46e10efb203c8a53c678e2218c8"}
```

### 6. Upload

Send `Content-Type: multipart/form-data` with one file part named `file`, filename
`payload.bin`, and part content type `application/octet-stream`. Its contents are
the four ASCII bytes `abcd` repeated `1024` times, for a total of `4096` bytes.
Hash only the file part's raw bytes, not the multipart headers or boundaries.
Return:

```json
{"sha256":"1f91053dcf43206eb082c0962785d35d86d4f629345f8bff25be7394416db908"}
```

## Validation

Run `bundle exec rspec .spec` against each implementation before comparing its
scenario results. The shared `.spec/v2/scenario_spec.rb` verifies all six requests,
including status, response body, JSON structure, item order, and digest values.
The existing `.spec/v1/route_spec.rb` also checks the legacy `GET /` route.
During migration, `.spec/v2/spec_helper.rb` lists the implementations that run
the v2 tests; other implementations skip them.

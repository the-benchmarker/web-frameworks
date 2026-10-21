# Requirements

Each framework included in here **MUST** respect some practices in order to have _meaningfull_ results

+ Fully disable logging capability
+ Disable TLS support
+ Keep responses uncompressed :information_source: HTTP request explictly disallow compression


# Implementation

Implement these six routes on port `3000` using the framework's normal routing,
parameter extraction, JSON, and multipart facilities. No database, authentication,
session, external service, or background job is needed.

| Request | HTTP 200 response |
| --- | --- |
| `GET /` | Empty body |
| `GET /user/00000042` | `{"id":"00000042"}` |
| `POST /user` | Empty body |
| `GET /serialization?seed=abcdefgh` | An array of 100 `{"seed":"abcdefgh"}` objects |
| `POST /deserialization` | `{"count":100,"checksum":4950}` for the shared JSON fixture |
| `POST /upload` | `{"first_byte":97,"last_byte":100,"checksum":"..."}` for the shared upload fixture |

## Requirements


## 1. Static routing

`GET /` returns an empty body, returns an HTTP `200`

## 2. Dynamic routing

`GET /user/{id}` reads the decoded route parameter and returns `{id}`.
Require exactly eight ASCII digits. Keep the ID as a string so leading zeros
survive. Reject invalid IDs with `400`, or `404` if routing rejects the path.
Query or body parameters cannot replace the route ID.

:information_source: ID is generated dynamically on v2 so **cache** does not mess-up with results

## 3. Method dispatch

`POST /user` with an empty body returns an empty body, returns HTTP `200`

## 4. JSON serialization

`GET /serialization?seed=abcdefgh` returns a JSON array of exactly 100 objects,
each containing only `{"seed":"abcdefgh"}`. Build and serialize the array on
every request; no fixture is needed.

Accept exactly one query parameter named `seed`, containing exactly eight
lowercase ASCII letters. Missing, invalid, array/object, repeated, or extra
query parameters return `400`. A body parameter cannot supply or replace the seed.

:information_source: Seed is generated dynamically on v2 so **cache** does not mess-up with results

## 5. JSON deserialization

`POST /deserialization` accepts `Content-Type: application/json` and no query
parameters. Parse a JSON object containing an `items` array of exactly 100 objects.
Each item must contain a numeric, integral `id` between `0` and `1000000`, inclusive.
Both `2` and `2.0` are valid; strings, booleans, null, and fractional values are not.
Other fields may be ignored.

Return the item count and the sum of the IDs as `checksum`. For the shared
[deserialization.json](deserialization.json) request body, return:

```json
{"count":100,"checksum":4950}
```

Query parameters, malformed JSON, missing fields, invalid items, and incorrect
array lengths return `400`. No seed is required.

## 6. Multipart upload

`POST /upload` accepts `multipart/form-data`. Require exactly one uploaded file
part named `file`; no seed is required. Read it through the framework's normal
uploaded-file API. Other form fields may be ignored. A query parameter or text
field cannot supply the file. Missing or repeated file parts and malformed
multipart bodies return `400`.

Read the first and last file bytes as unsigned integers and compute a lowercase
SHA-256 digest of every uploaded byte. Return only `first_byte`, `last_byte`,
and `checksum`. For the shared [upload.bin](upload.bin) file, return:

```json
{"first_byte":97,"last_byte":100,"checksum":"1f91053dcf43206eb082c0962785d35d86d4f629345f8bff25be7394416db908"}
```

For an empty file, both byte samples are `null` and the checksum is the SHA-256
of an empty byte string. Clean up temporary upload files after each request,
including validation and parsing failures.

## Validation and load requests

Set `specs: v2` at the top level of the framework's `config.yaml` to select this
contract. Rails currently uses v2; other frameworks default to `specs: v1`.
RSpec loads [.spec](.spec) and selects [.spec/v1](.spec/v1) or [.spec/v2](.spec/v2)
from the framework config, excluding the other suite. To validate a running Rails server:

```bash
LANGUAGE=ruby FRAMEWORK=rails BENCHMARK_URL=http://127.0.0.1:3000 bundle exec rspec
```

The Lua scripts in [.load_scripts](.load_scripts/README.md) generate load requests.
They vary path IDs and serialization seeds, and send the exact root
`deserialization.json` and `upload.bin` bytes for the POST workloads. The server
must process the supplied bodies, rather than read these fixtures itself.

The v1 suite keeps the historical contract (`GET /`, `GET /user/0`, and
`POST /user`). Servers without a supported framework facility report that workload as
unsupported; performance results require passing the corresponding specs.

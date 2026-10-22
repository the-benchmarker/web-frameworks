# Benchmark scenario (v2)

Implement these six routes on port `3000` using the framework's normal routing,
parameter extraction, JSON, and multipart facilities. No database, authentication,
session, external service, or background job is needed.

Select this contract with top-level `specs: v2` in the framework's `config.yaml`.
Omitting `specs` also selects v2. The legacy `specs: v1` contract is described
at the end of this document. [Rails](ruby/rails) is the v2 reference implementation.

## Runtime requirements

Every implementation must satisfy these requirements during measurement:

- Disable application and HTTP server logging, including access and error logs.
- Serve plain HTTP on port `3000`. Disable TLS and redirects to HTTPS.
- Disable response compression. Do not send a `Content-Encoding` header, even
  when a client advertises `gzip`, `br`, or `zstd`. Load requests send
  `Accept-Encoding: identity`; correctness tests also advertise compression.
- Compute data-dependent responses from each request. Do not cache completed
  responses, return precomputed payloads, or read the shared fixtures on the
  server to substitute for request processing. Conditional headers such as
  `If-None-Match: *` must not bypass the workload with a `304` response.

Framework-specific tuning must preserve these requirements and the work defined
for each route below.

## Response and request conventions

- Successful requests return exactly HTTP `200`. The two empty responses have
  a zero-length body; an empty JSON object or JSON string is not equivalent.
- JSON responses use `Content-Type: application/json`, optionally with
  `charset=utf-8`. Return only the fields shown for each route. JSON whitespace
  and object key order do not matter.
- Invalid inputs return the status specified below. Error response bodies and
  their media types are not prescribed, but they must remain uncompressed.
- Both JSON deserialization and multipart uploads must accept bodies sent with
  `Content-Length` or HTTP/1.1 chunked transfer encoding. A missing
  `Content-Length` on a chunked request does not mean the body is empty.
- Request-size limits must accommodate the load payloads documented below,
  including multipart framing. Fixture sizes are not maximum accepted sizes;
  this scenario does not prescribe a universal request-size limit.

## Routes at a glance

The IDs, seeds, and fixture responses in this table are examples. Valid requests
with other inputs must produce the corresponding results.

| Request | HTTP 200 response |
| --- | --- |
| `GET /` | Empty body |
| `GET /user/00000042` | `{"id":"00000042"}` |
| `POST /user` | Empty body |
| `GET /serialization?seed=abcdefgh` | An array of 100 `{"seed":"abcdefgh"}` objects |
| `POST /deserialization` | `{"count":100,"checksum":4950}` for the shared JSON fixture |
| `POST /upload` | `{"first_byte":97,"last_byte":100,"checksum":"..."}` for the shared upload fixture |

## 1. Static routing

`GET /` returns an empty body with HTTP `200`.

## 2. Dynamic routing

`GET /user/{id}` reads the decoded route parameter and returns a JSON object
containing only `id`. For example, `/user/00000042` returns
`{"id":"00000042"}`.

Require exactly eight ASCII digits (`0` through `9`). Keep the ID as a string
so leading zeros survive. Decode percent-encoded path characters before
validation: `/user/%30%30%30%30%30%30%34%32` has ID `00000042`.

Reject invalid IDs with `400`, or `404` if routing rejects the path. Read the ID
from the route parameters; query or body parameters cannot replace it.

## 3. Method dispatch

`POST /user` with an empty request body returns an empty body with HTTP `200`.

## 4. JSON serialization

`GET /serialization?seed=abcdefgh` returns a JSON array of exactly 100 objects,
each containing only `{"seed":"abcdefgh"}`. Build and serialize the array on
every request; no fixture is needed.

Accept exactly one query parameter named `seed`, containing exactly eight
lowercase ASCII letters. Missing, invalid, array/object, repeated, or extra
query parameters return `400`. Check for repeated names before the framework
collapses them into a single value, including percent-encoded names such as
`seed=abcdefgh&%73eed=zzzzzzzz`.

Read the seed from the query parameters. A body parameter cannot supply a
missing seed or replace a valid query seed. For example, a query seed of
`abcdefgh` with a JSON body of `{"seed":"zzzzzzzz"}` uses `abcdefgh`.

## 5. JSON deserialization

`POST /deserialization` accepts `Content-Type: application/json`, including
`application/json; charset=utf-8`, and no query parameters. A missing or
unsupported `Content-Type` returns `415`, even if `Accept: application/json`
is present: `Accept` describes the desired response, not the request body.

Parse the supplied body as a JSON object containing an `items` array of exactly
100 objects.
Each item must contain a numeric, integral `id` between `0` and `1000000`, inclusive.
Both `2` and `2.0` are valid; strings, booleans, null, and fractional values are not.
Other fields in the object and its items may be ignored.

Return only `count` (the item count) and `checksum` (the sum of all IDs). For the shared
[deserialization.json](deserialization.json) request body, return:

```json
{"count":100,"checksum":4950}
```

Query parameters, malformed JSON, missing fields, invalid items, and incorrect
array lengths return `400`. An empty body or a top-level array, scalar, or null
also returns `400`. No seed is required or accepted in the query.

## 6. Multipart upload

`POST /upload` accepts `Content-Type: multipart/form-data; boundary=...`.
A missing or unsupported media type returns `415`. A multipart media type
with a missing or invalid boundary, or a malformed multipart body, returns `400`.
Accept different valid boundaries; do not hard-code the fixture's boundary.

Require exactly one part named `file`, and it must be an uploaded file.
Read it through the framework's normal uploaded-file API. Other form fields
may be ignored; no seed is required. A query parameter or text field cannot
supply the file. Missing or repeated `file` parts return `400`; do not silently
select the first or last repeated part. The uploaded filename and file media
type may differ from those of the fixture.

Read the first and last file bytes as unsigned integers from `0` to `255` and
compute a lowercase SHA-256 digest of every uploaded byte. Hash only the file
contents, excluding multipart headers and boundaries. Return only `first_byte`,
`last_byte`, and `checksum`. For the shared [upload.bin](upload.bin) file, return:

```json
{"first_byte":97,"last_byte":100,"checksum":"1f91053dcf43206eb082c0962785d35d86d4f629345f8bff25be7394416db908"}
```

For an empty file, both byte samples are `null` and the checksum is the SHA-256
of an empty byte string. Clean up temporary upload files after each request,
including validation and parsing failures.

## Correctness validation

Run the shared [.spec](.spec) suite from the repository root against a running
server. For example, to validate Rails:

```bash
LANGUAGE=ruby FRAMEWORK=rails BENCHMARK_URL=http://127.0.0.1:3000 bundle exec rspec .spec
```

Replace `LANGUAGE`, `FRAMEWORK`, and `BENCHMARK_URL` for the implementation under
test. RSpec selects [.spec/v1](.spec/v1) or [.spec/v2](.spec/v2) from the
framework config and excludes the other suite. It defaults to v2 if no framework
is selected. Check that the command actually runs examples; zero examples does
not validate an implementation.

Pass the full selected correctness suite before collecting results. `ROUTES`
limits which workloads are measured; it does not reduce the correctness suite.
If a framework cannot support a required facility, report that workload as
unsupported rather than replacing its work with a fixed response. An incomplete
implementation does not qualify for a full v2 result.

## Load requests and fixtures

The Lua scripts in [.load_scripts](.load_scripts) generate load requests using
zrk 3.0.0 or newer. Warmup and measurement use the same scripts. They vary path
IDs, serialization seeds, and POST bodies:

- Deserialization preserves the 100-item fixture structure and other fields,
  converts every ID to a six-digit number, and varies five IDs using a random
  prefix, thread ID, and counter. IDs stay between `100000` and `999999`; JSON
  numbers cannot be padded with leading zeros. The body length stays constant.
- Upload repeats the 4 KiB fixture to produce a 256 KiB file, replacing its first
  30 bytes with the same kind of fixed-width token on each request. Both the file
  size and multipart framing stay constant. The complete HTTP request body is
  larger than 256 KiB because it also includes that framing.

The counter prevents repeated bodies within a thread, the thread ID separates
threads, and a random prefix read once per Lua state separates runs. Fixture
files and their expected spec responses remain unchanged. The server must
process the supplied bodies, rather than read these fixtures itself.

These workloads use `setup(thread)`, `init(args)`, and `request()` without a
`response()` hook. Check client CPU usage and zrk's `workload.next` and
`workload.thread_share` metrics when measuring generator overhead; those hook
metrics exclude zrk's request copying/parsing and lock waiting. Use HTTP/1.1 or
HTTP/2 for uploads: zrk 3.0.0 limits HTTP/3 requests to about 4 KiB including
headers. See the [zrk release notes](https://github.com/zoxy-io/zrk/releases/tag/v3.0.0).

## Legacy v1 contract

With `specs: v1`, the shared suite checks only `GET /`, `GET /user/0`, and
`POST /user`. Responses may use any successful `2xx` status. The root and POST
responses are empty; `/user/0` returns the plain string `0`, not a JSON object.
The default v1 load workload measures only `GET /`. Keep v1 and v2 results
separate because they perform different work.

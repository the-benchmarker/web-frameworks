# Benchmark scenario

Implement these six routes on port `3000` using the framework's normal routing,
parameter extraction, JSON, and multipart facilities. No database, authentication,
session, external service, or background job is needed.

##### Disclaimer

This project is in heavy development. The existing UI https://web-frameworks-benchmark.vercel.app correspond to 3 routes

- GET / -> Empty body
- POST /user -> Empty body
- GET /user/:id -> Return the :id

The first two routes are mainly overhead, just testing **routing** feature of a framework. Some work have been done in https://github.com/the-benchmarker/web-frameworks/pull/9864 to update this current scenario.

This two routes are only here for compatibility with the results in current UI, but any new implementation must define 6 routes.

## Runtime requirements

Every implementation must satisfy these requirements during measurement:

- Disable application and HTTP server logging, including access and error logs.
- Serve plain HTTP on port `3000`. Disable TLS and redirects to HTTPS.
- Disable response compression. Do not send a `Content-Encoding` header
- Compute data-dependent responses from each request.

## Response and request conventions

- Successful requests return exactly HTTP `200`. The two empty responses have
  a zero-length body; an empty JSON object or JSON string is not equivalent.
- JSON responses use `Content-Type: application/json`, optionally with
  `charset=utf-8`. Return only the fields shown for each route.
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

Require an eight-character string of ASCII digits (`0` through `9`), including
`00000000`. IDs do not have to represent strictly positive numbers. Keep the ID
as a string so leading zeros survive. Decode percent-encoded path characters before
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

## 5. JSON deserialization

`POST /deserialization` accepts JSON request bodies and no query parameters.
Accept all standard JSON media types: `application/json` and media types using
the `+json` structured syntax suffix, such as `application/problem+json`.
Also accept the legacy JSON aliases `text/x-json` and `application/jsonrequest`.
Accept `application/json; charset=utf-8` as well. These media types all use the
same benchmark body schema below. A missing or
unsupported `Content-Type` returns `415`, even if `Accept: application/json`
is present: `Accept` describes the desired response, not the request body.
Check the media type before parsing the body, so malformed bodies with an
unsupported media type also return `415`.

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
Check the media type before parsing the body; an unsupported media type still
returns `415` when its body is malformed.
Accept different valid boundaries; do not hard-code the fixture's boundary.

Require exactly one part named `file`, and it must be an uploaded file.
Read it through the framework's normal uploaded-file API. Other form fields
may be ignored; no seed is required. A query parameter or text field cannot
supply the file. Missing or repeated `file` parts return `400`; do not silently
select the first or last repeated part. The uploaded filename and file media
type may differ from those of the fixture.

Parts with `filename=""` represent unselected file inputs and may be ignored,
including when counting repeated `file` parts. If no uploaded file remains,
return `400`. A zero-byte file with a nonempty filename is an uploaded file
and must be accepted.

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

## Load payloads

The shared fixtures provide correctness examples. Measurement uses the following
payloads, whose contents vary between requests:

- JSON deserialization: the shared fixture is 10,332 bytes. The
  [load script](.load_scripts/deserialization.lua) expands every ID to six digits
  and replaces the first five IDs with values from a per-request token. The
  resulting request body is 10,742 bytes and still contains exactly 100 items.
  Compute the checksum from the supplied IDs, not the fixture's example sum.
- Multipart upload: the shared file is 4,096 bytes. The
  [load script](.load_scripts/upload.lua) repeats it 64 times to create a
  262,144-byte (256 KiB) file and replaces its first 30 bytes with a per-request
  token. The HTTP request body is larger because it includes multipart headers
  and boundaries. Request-size limits must accommodate the file plus this
  framing. Compute byte samples and the digest from the uploaded contents.

These sizes describe the current load scripts; they are not maximum accepted
request sizes.

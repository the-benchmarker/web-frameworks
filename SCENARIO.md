# Stateless API benchmark (v2)

This scenario measures six HTTP workloads through a framework's normal request
and response facilities. Applications listen on port `3000`. Requests carry
their own input; the application needs no database, authentication, session,
cache, template, external service, or background job.

| # | Request | Feature | Successful response |
|---|---|---|---|
| 1 | `GET /health` | Static routing | Empty body |
| 2 | `GET /user/{user}` | Dynamic routing and path decoding | Decoded path value as plain text |
| 3 | `POST /user` | HTTP method dispatch | Empty body |
| 4 | `GET /serialization?seed={seed}` | Query extraction and JSON serialization | Seed and 100 fixed records |
| 5 | `POST /deserialization` | JSON decoding and access to parsed values | Seed, item count, and ID sum |
| 6 | `POST /upload` | Multipart parsing and form/file binding | Seed, file metadata, size, and two byte samples |

All successful responses have status `200`. Empty bodies have zero bytes.
JSON responses use `Content-Type: application/json` and are compared as parsed
JSON: whitespace and object key order do not matter. Array order does matter.
Every seed is a **string of exactly eight ASCII digits**, such as `"00000042"`.

## Try the Rails reference

Run these commands from the repository root after installing Ruby, Bundler,
Docker, and the root Gemfile dependencies:

```bash
LANGUAGE=ruby FRAMEWORK=rails COMPLETE=true bundle exec rake config
make -f ruby/rails/.Makefile build.puma
export BENCHMARK_URL="http://$(cat ruby/rails/ip-puma.txt):3000"
make -f ruby/rails/.Makefile test.puma
```

The generator prepares the shared dataset, Dockerfiles, requests, and Makefile
for Rails and starts the container in v2 mode. The test target runs
[.spec/v2](.spec/v2) against the container. See the
[Rails README](ruby/rails/README.md) for the full Docker workflow.
Run the request examples below while the container is running, using its
`BENCHMARK_URL` from the command above.
Stop the container afterward with `make -f ruby/rails/.Makefile unbuild.puma`.

## 1. Static routing

`GET /health` returns `200` with an empty body.

```bash
curl -i "$BENCHMARK_URL/health"
```

## 2. Dynamic routing

`GET /user/{user}` returns the decoded UTF-8 path segment with
`Content-Type: text/plain; charset=utf-8`.

| Request | Response body |
|---|---|
| `GET /user/00000042` | `00000042` |
| `GET /user/alice` | `alice` |
| `GET /user/alice.smith` | `alice.smith` |
| `GET /user/caf%C3%A9` | `café` |

Extract the route parameter through the framework. Correctness checks use
multiple values, including Unicode. Varying-input load runs use fixed-width
numeric values so response sizes stay equal.

## 3. Method dispatch

`POST /user` with an empty body returns `200` with an empty body.
It intentionally stores no user; it exercises dispatch by HTTP method.

```bash
curl -i -X POST "$BENCHMARK_URL/user"
```

## 4. JSON serialization

`GET /serialization?seed=00000042` returns a JSON object with:

- `seed`: the supplied seed string.
- `items`: all 100 objects in the [shared dataset](.tasks/fixtures/serialization.json),
  in their original order.

Load the dataset once as immutable objects. Encode the response through the
framework's ordinary JSON response facility on every request. Copy the seed
once into the response; do not generate or transform each record.

The first object in `items` is:

```json
{"id":0,"name":"User 0","active":true,"tags":["api","benchmark"],"profile":{"score":0,"note":null}}
```

The fixture exercises integers, strings, booleans, arrays, nested objects, and
null. IDs run from `0` through `99`. The record count is always 100;
this workload has no `n` parameter.

Missing, invalid, array/object, or repeated query seeds return `400`.
Correctness compares every fixture field and verifies that changing the seed
changes the response seed.

## 5. JSON deserialization

`POST /deserialization` accepts `Content-Type: application/json` with an
object containing a valid seed and an `items` array of zero through 100 objects.
Every item must have an integer `id` from `0` through `1000000`, inclusive.
Other item fields are parsed normally and may be ignored in the summary.

```bash
curl "$BENCHMARK_URL/deserialization" \
  -H 'Content-Type: application/json' \
  -d '{"seed":"00000042","items":[{"id":2},{"id":7}]}'
```

Expected response:

```json
{"seed":"00000042","count":2,"idSum":9}
```

The [performance request](.tasks/fixtures/v2-deserialization.json) contains the
100 fixture records and produces:

```json
{"seed":"00000042","count":100,"idSum":4950}
```

An empty array produces `count: 0` and `idSum: 0`.
The small ID sum demonstrates access to decoded input without a hashing workload.

Return `400` for malformed JSON, missing required fields, invalid seeds,
non-array or oversized `items`, non-object items, or invalid IDs.
Strings, booleans, null, and fractional/float IDs are rejected rather than coerced.
Required values come from the body; query parameters cannot fill missing fields.
Unsupported request media types return `415`.

## 6. Multipart parsing

`POST /upload` accepts `multipart/form-data` containing exactly one text
field named `seed` and one file part named `file`. Read both through the
framework's normal parsed-parameter and uploaded-file APIs. Query parameters
cannot provide the required body fields.

The performance file is [payload.bin](.tasks/fixtures/payload.bin): 4096 bytes
consisting of `abcd` repeated 1024 times.

```bash
curl "$BENCHMARK_URL/upload" \
  -F 'seed=00000042' \
  -F 'file=@.tasks/fixtures/payload.bin;filename=payload.bin;type=application/octet-stream'
```

Expected response:

```json
{"seed":"00000042","filename":"payload.bin","contentType":"application/octet-stream","size":4096,"firstByte":97,"lastByte":100}
```

`size` counts only file bytes, excluding multipart headers and boundaries.
The two byte samples are unsigned integers from `0` through `255`; an empty
file returns zero size and null for both samples. Read the samples from the file,
rather than inferring them from its filename or declared media type.

There is no hashing or application-managed file storage. Normal parser buffering
to a temporary file is part of the measured stack and must be documented.
Correctness varies the seed, boundary, filename, size, and binary byte samples.

Malformed multipart data, invalid seeds, or missing/repeated required parts
return `400`. Unsupported request media types return `415`.
An entry without supported multipart handling reports this scenario as N/A.

## Implementing and comparing frameworks

Use each framework's ordinary routing, parameter extraction, JSON, and multipart
facilities. Record the server/runtime and parsing/encoding components, including
their versions. A framework may delegate to its server or a documented library;
the result measures that configured HTTP stack. Do not substitute handwritten
parsers or precomputed JSON/HTTP responses.

Compare results per scenario, using the same fixtures, input sequence, payload
sizes, hardware, concurrency, and benchmark revision. Show implementation status
as passed, pending, unsupported, or failed; only passing scenarios receive
performance results. Do not reward missing features through a reduced aggregate.

Validate positive and negative cases before collecting results. The shared tests
include changing inputs at the same URL, invalid fields, duplicate query/form
values, and chunked JSON/multipart bodies. Error bodies follow the framework's
normal error handling; their byte representation is not prescribed.

Rails additionally limits JSON and multipart bodies to 64 KiB, including chunked
requests; larger bodies return `413`. It emits `Cache-Control: no-store` on
normal controller responses and uses Rack's temporary-upload cleanup.
See [the Rails implementation notes](ruby/rails/README.md).

## Scenario versions and load inputs

There are two versions: v1 is the original three-route baseline, and v2 is
this six-route contract. Select v2 with `COMPLETE=true bundle exec rake config`;
omitting `COMPLETE` or setting it to `false` selects v1. `COMPLETE` is the only
route-version selector. The generator configures Docker startup and selects
the matching test suite and JSON/multipart requests. Regenerate and rebuild
the container when switching versions.

Rails implements both versions. Its `GET /` compatibility route is outside
the six v2 workloads; the v2 health route is `/health`.
Keep results from different versions separate.

The current generated load commands use fixed inputs. Varied correctness checks
catch hardcoded answers, but cannot rule out a correctly keyed response cache.
When the pinned load generator supports varying inputs, use the same reproducible,
fixed-size sequence of path values, seeds, JSON IDs, and upload samples for every
entry. Label fixed-input runs accordingly.

# Stateless API benchmark (v2)

This contract defines six HTTP workloads through a framework's normal request
and response facilities. Its goal is to compare framework features with minimal
application logic. Applications listen on port `3000`. Requests carry their own
input; the application needs no database, authentication, session, cache,
template, external service, or background job.

End-to-end HTTP measurements include the language runtime, HTTP server, and
JSON/multipart components. Compare frameworks within a matched runtime/server
configuration where possible. Cross-language results describe the configured
HTTP stacks; they cannot isolate framework overhead from the language runtime.

Following the [fixed-dataset proposal in PR #7737](https://github.com/the-benchmarker/web-frameworks/pull/7737#issuecomment-6061853811),
keep application computation small and bounded. `/compute`, database access,
hashing, and per-record seed arithmetic are outside this contract. Loading
fixtures once and encoding them on each request exercises framework features
without adding an application algorithm to the comparison.

Shared v2 specs and load scripts implement this contract. Individual frameworks
must pass the selected suite before their results are collected.

| # | Request | Feature | Successful response |
|---|---|---|---|
| 1 | `GET /health` | Static routing | Empty body |
| 2 | `GET /user/{user}` | Dynamic routing and path decoding | Decoded path value as plain text |
| 3 | `POST /user` | HTTP method dispatch | Empty body |
| 4 | `GET /serialization?seed={seed}` | Query extraction and JSON serialization | Seed and 100 fixed records |
| 5 | `POST /deserialization` | JSON decoding and access to parsed values | Item count and ID sum |
| 6 | `POST /upload` | Multipart parsing and form/file binding | Seed, file metadata, size, and two byte samples |

All successful responses have status `200`. Empty bodies have zero bytes.
JSON responses use `Content-Type: application/json`, with an optional UTF-8
charset parameter, and are compared as parsed JSON: whitespace and object key
order do not matter. Array order and every fixture field do matter.
Seeds for serialization and upload are **strings of exactly eight lowercase
ASCII letters** (`[a-z]{8}`), such as `"abcdefgh"`. Reject other types and
characters rather than coercing them. Deserialization has no seed.

## HTTP transport and compression

**HTTP responses must be uncompressed by design.** Disable response compression
in the framework, middleware, HTTP server, and any proxy in the measured path.
This applies to every route and to successful and error responses. Do not use
gzip, Brotli, deflate, zstd, precompressed assets, or other content encodings.
The `Content-Encoding` response header must be absent.

Correctness checks, warmup, and measured requests must all send
`Accept-Encoding: identity`. Disable automatic response decompression in the
validation client and load generator so they cannot hide compression. Request
bodies are also sent uncompressed, without `Content-Encoding`.

Validation must inspect the response headers and uncompressed body bytes on
every workload. Additionally, repeat checks with
`Accept-Encoding: gzip, br, zstd` to verify that compression remains disabled
even when a client advertises support. Any compressed response fails validation.
HTTP chunked transfer framing is allowed; it is distinct from content compression.

Use the same HTTP version and keep-alive policy for compared runs. Measure the
application directly where possible; document any proxy and its configuration.
For the examples below, set `BENCHMARK_URL` to a running implementation's base
URL, such as `http://127.0.0.1:3000`. These examples request identity encoding;
do not add curl's `--compressed` option.

## 1. Static routing

`GET /health` returns `200` with an empty body.

```bash
curl -i -H 'Accept-Encoding: identity' "$BENCHMARK_URL/health"
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

Extract the route parameter through the framework and return it as a string.
Do not convert numeric-looking values to numbers: `00000042` must retain its
leading zeros. Validation must use multiple values, including Unicode.
Varying-input load runs use eight-digit ASCII values so response sizes stay equal.

## 3. Method dispatch

`POST /user` with an empty body returns `200` with an empty body.
It intentionally stores no user; it exercises dispatch by HTTP method.

```bash
curl -i -X POST -H 'Accept-Encoding: identity' "$BENCHMARK_URL/user"
```

## 4. JSON serialization

`GET /serialization?seed=abcdefgh` returns a JSON object with:

- `seed`: the supplied seed string.
- `items`: all 100 objects in the [shared dataset](serialization.json),
  in their original order.

Load the dataset once as immutable objects. Encode the response through the
framework's ordinary JSON response facility on every request. Copy the seed
once into the response; do not generate or transform each record.

Keep this dataset once at the repository root. The Docker runner injects it
read-only at `/serialization.json` and sets `SERIALIZATION_FIXTURE` to that
path. Do not duplicate the fixture in individual framework directories or images.

The first object in `items` is:

```json
{"id":0,"name":"User 0","active":true,"tags":["api","benchmark"],"profile":{"score":0,"note":null}}
```

The fixture exercises integers, strings, booleans, arrays, nested objects, and
null. IDs run from `0` through `99`. The record count is always 100;
there is no `n` or `count` parameter. Accept exactly one query parameter, `seed`;
additional query keys and repeated seeds return `400`.

Missing, invalid, array/object, or repeated query seeds return `400`.
Validation must compare every fixture field and verify that changing the seed
changes the response seed.

## 5. JSON deserialization

`POST /deserialization` has no query parameters. It accepts
`Content-Type: application/json` with an object containing an `items` array of
exactly 100 objects. It does not require or return a seed.
Every item must have a JSON numeric `id` with an integral value from `0` through
`1000000`, inclusive. Validate the numeric value independently of the runtime's
integer/float representation: `2` and `2.0` both represent a valid ID of two.
Other item fields are parsed normally and may be ignored in the summary.

```bash
ruby -rjson -e 'puts JSON.generate(items: JSON.parse(File.read("serialization.json")))' | \
curl "$BENCHMARK_URL/deserialization" \
  -H 'Accept-Encoding: identity' \
  -H 'Content-Type: application/json' \
  --data-binary @-
```

Expected response:

```json
{"count":100,"idSum":4950}
```

The [deserialization Lua script](.load_scripts/deserialization.lua) loads all 100
shared records on the client and prepares equal-sized bodies with different
three-digit IDs. It sends only the JSON body, without query parameters or seed.
All other fixture fields are preserved.

Compute the count and sum from the parsed body on each request. The sum requires 100 additions
and is bounded by `100000000`, avoiding large-number arithmetic or hashing.

Return `400` for query parameters, malformed JSON, missing required fields,
non-array `items`, any array length other than 100, non-object items, or invalid IDs.
String, boolean, null, and non-integral IDs are rejected rather than coerced.
Required values come from the body; query parameters cannot fill missing fields.
Unsupported request media types return `415`.

## 6. Multipart parsing

`POST /upload` accepts `multipart/form-data` containing exactly one text
field named `seed` and one file part named `file`. Read both through the
framework's normal parsed-parameter and uploaded-file APIs. Query parameters
cannot provide the required body fields.

The [upload Lua script](.load_scripts/upload.lua) creates a 4096-byte file body
consisting of `abcd` repeated 1024 times on the client, without a separate fixture file.

```bash
ruby -e 'STDOUT.write("abcd" * 1024)' | \
curl "$BENCHMARK_URL/upload" \
  -H 'Accept-Encoding: identity' \
  -F 'seed=abcdefgh' \
  -F 'file=@-;filename=payload.bin;type=application/octet-stream'
```

Expected response:

```json
{"seed":"abcdefgh","filename":"payload.bin","contentType":"application/octet-stream","size":4096,"firstByte":97,"lastByte":100}
```

`size` counts only file bytes, excluding multipart headers and boundaries.
The two byte samples are unsigned integers from `0` through `255`; an empty
file returns zero size and null for both samples. Read the samples from the file,
rather than inferring them from its filename or declared media type.

There is no hashing or application-managed file storage. Normal parser buffering
to a temporary file is part of the measured stack and must be documented.
Validation must vary the seed, boundary, filename, size, and binary byte samples.
Use simple filenames without directory paths; include zero-length files and
byte values above `127` in correctness checks.

Malformed multipart data, invalid seeds, or missing/repeated required parts
return `400`. Unsupported request media types return `415`.
An entry without supported multipart handling reports this scenario as N/A.

## Implementation and validation requirements

Use each framework's ordinary routing, parameter extraction, JSON, and multipart
facilities, including integrations documented by that framework. Delegation to
the underlying server or a supported library is allowed and must be recorded.
An entry without a supported facility reports the affected workload as N/A.
Do not substitute handwritten parsers or a bespoke path that bypasses the
framework's request/response facilities.

Preload immutable objects outside the measured phase. Parse request bodies and
encode JSON responses on every request; do not cache completed responses,
pre-encode the dataset, splice a seed into precomputed JSON, or construct fixed
HTTP response frames. Framework optimizations that preserve the specified work
are allowed.

Before collecting performance results, validation must cover:

- Every successful response's status, media type, complete body, and absence of
  content encoding, including when compression is advertised by the client.
- Changing path values and seeds, and different JSON/multipart bodies sent to
  the same URL; expected summaries must be calculated from each input.
- Malformed JSON and multipart bodies, missing and invalid fields, repeated
  query seeds or required multipart parts, and unsupported request media types.
- JSON and multipart requests with chunked transfer encoding as well as a
  declared content length.
- A common 64 KiB (`65536` byte) limit on JSON and multipart request bodies;
  larger bodies return `413`. Count multipart headers and boundaries, exclude
  HTTP transfer framing, and enforce the limit on received bytes for chunked
  requests as well as on declared content length.

The endpoint sections define `400` and `415` cases. Error bodies follow the
framework's normal error handling; their exact bytes are not prescribed, and
they must remain uncompressed. Run negative cases during correctness checks;
measured runs use valid inputs. HTTP protocol compliance beyond these checks
requires a separate assessment.

## Comparing and reporting results

Publish results per workload. Keep the static routing and method-dispatch
baselines visible alongside the JSON and multipart workloads. Do not combine
them into a single framework ranking or average over only implemented routes.
Show each workload's status as passed, pending, unsupported (N/A), or failed;
only a passing workload receives performance results. Missing features must
never improve a score.

For every compared run, keep the same benchmark revision, scenario version,
fixtures, input mode and sequence, payload sizes, hardware, CPU/memory budgets,
HTTP version, keep-alive policy, concurrency, and measurement duration. Disable
compression in every entry. Use production configuration and document worker
counts and relevant middleware. Match runtime and server versions within
comparison groups where possible; identify remaining stack differences.

Warm up each workload until startup and JIT effects have settled, then repeat
measurements. Report the median and spread across runs, retain individual runs,
and avoid choosing only the fastest result. Record throughput, latency
percentiles, errors, timeouts, and server/load-generator resource usage. Report
fixed-rate latency separately from latency in a closed-loop throughput run,
and state whether its rate is absolute or relative to each entry's throughput.

Keep load generation on separate resources and verify that it and the network
can sustain the offered load. CPU saturation is diagnostic rather than proof
of validity: a single-threaded server can saturate one core while total CPU
utilization stays low. Identify bottlenecks before drawing conclusions from
throughput differences.

Record framework, runtime, server, JSON encoder/decoder, multipart parser, and
load-generator versions, plus the application configuration. Document parser
buffering and temporary-file behavior, and use normal upload cleanup. These
components are part of the measured stack.

## Scenario versions and load inputs

V1 is the historical three-route contract: `GET /`, `GET /user/0`, and
`POST /user`. Preserve its route and response semantics. V2 is the six-route
contract in this document; its health route is `/health`. A v2 implementation
may retain `GET /` for v1 compatibility, outside the six v2 workloads. Keep
versions separate and record load-model or input changes as well; an unchanged
route name alone does not make historical results comparable.

`COMPLETE=false` (the default) and `COMPLETE=true` select v1/v2 generation in
[.tasks/config.rake](.tasks/config.rake). The generated container startup and
RSpec commands use the same mode. The full selected contract must pass;
`ROUTES` can select a subset of workloads to measure.

Label runs as **fixed-input** or **varying-input**. Fixed-input runs must be
identified as susceptible to response caching. Varied correctness checks catch
hardcoded answers but cannot rule out a cache keyed on the full request. The
requirement to parse and encode each request still applies.

For varying-input runs, prepare the same reproducible sequence for every entry
outside the measured server work. Keep path values eight digits wide and seeds
eight lowercase letters wide, JSON IDs within fixed digit widths, and uploaded files at 4096 bytes so payload
sizes remain equal. Vary upload byte samples without adding hashing. Pin the
load-generator version and script, and verify sufficient generator capacity.
Do not mix fixed-input and varying-input results in the same comparison.

## Implementation status

The shared specs live in [.spec](.spec), with the legacy suite selected by
default and the complete suite selected by `COMPLETE=true`. They cover all six
workloads, invalid inputs, media types, chunked requests, the 64 KiB body limit,
and responses that stay uncompressed when compression is advertised.

Request generation lives in [.load_scripts](.load_scripts/README.md), with no
framework-local copies of the JSON fixture or upload payload. Warmup, closed-loop
collection, and optional latency passes use the same per-workload scripts.

To validate an already running complete implementation:

```bash
COMPLETE=true BENCHMARK_URL=http://127.0.0.1:3000 bundle exec rspec .spec
```

Other implementations may still need v2 support. Do not publish their complete
scenario results until this suite passes for the measured revision and server
configuration. Specs belong in `.spec`, never in individual framework directories.

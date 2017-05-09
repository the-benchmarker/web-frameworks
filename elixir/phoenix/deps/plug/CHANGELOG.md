## Changelog

## v1.3.5

* Bug fixes
  * Validate cookie headers

## v1.3.4

* Bug fixes
  * Do not convert exits into errors

## v1.3.3

* Bug fixes
  * Support improper lists, bitstrings and structs in safe_binary_to_term

## v1.3.2

* Bug fixes
  * [Plug.Crypto] Provide safer deserialization mechanisms
  * [Plug.Static] Properly handle null bytes

## v1.3.1

* Enhancements
  * [Plug.Conn] Support arbitrary cookie attributes
  * [Plug.SSL] Support tuple hosts in Plug.SSL
  * [Plug.Static] Add ability to generate etags via a user defined function

* Bug fixes
  * [Plug.ErrorHandler] Do not unwrap errors wrapped in `Plug.Conn.WrapperError` when reraising

## v1.3.0

* Enhancements
  * [Plug.Adapters.Cowboy] Support tuples with other than 2 elements in the adapter options
  * [Plug.Adapters.Cowboy] Support controlling how multipart headers are parsed
  * [Plug.Conn] Add the `:path_params` field to access path params apart from the `params` field
  * [Plug.Conn.Status] Allow custom status codes to be configured and dynamically inflect their atom name
  * [Plug.Debugger] Support content negotiation and defaults to Markdown when HTML cannot be served
  * [Plug.Router] Extend `match/2` macros to accept a plug and options
  * [Plug.Router] Make path parameters available in `conn.params`
  * [Plug.Router] Add `:init_opts` option to `forward` macro for plug options
  * [Plug.Router] Add `:assigns` option to router macros to assign values to `conn.assigns`

* Bug fixes
  * [Plug.Debugger] Do not show query parameters when debugging a page with bad query string
  * [Plug.Parsers] Keep `body_params` unfetched if the content-type is allowed to pass through the parser

## v1.2.2

* Bug fix
  * Do not generate AST with line -1 on OTP >= 19

## v1.2.1

* Enhancements
  * Raise proper bad request and timeout exceptions on parse errors
  * Support environment hosts on Plug.SSL
  * Do not raise when nothing is plugged in a Plug.Builder

## v1.2.0

* Enhancements
  * Introduce new error page
  * Set default max connections to 16k instead of 1k

* Bug fixes
  * Ensure that `Plug.Conn`'s public API is consistent about not sending empty chunks

* Deprecations
  * Use the new `MIME` project instead of `Plug.MIME`
  * Introduce safer algorithms in `Plug.MessageEncryptor` and `Plug.MessageVerifier`. The previous ones will be supported for a year allowing safe migration.

* Backwards incompatible changes
  * Depend on Elixir ~> 1.2.3 or ~> 1.3

## v1.1.4

* Bug fixes
  * Fix `:only` and `:only_matching` which were not bypassing requests unless both were enabled

* Enhancements
  * Support reading of `conn.host` in `Plug.Test`
  * Add normalization of `:dhfile` for Cowboy's SSL options
  * Import error reporting and performance of `Plug.Static`

## v1.1.3

* Enhancements
  * Add `:only_matching` option to `Plug.Static`

* Bug fixes
  * Return 400 from `Plug.Static` on invalid paths
  * Ensure `Plug.Upload` does not error out on invalid access
  * Ensure query string errors return 400

## v1.1.2

* Enhancements
  * Raise on cookie overflow
  * Log (with :debug level) when session cookie cannot be decoded

* Bug fixes
  * Ensure Plug.Parsers fail with request too large even when read_length > length

## v1.1.1

* Enhancements
  * Add `:brotli` to `Plug.Static`

* Bug fixes
  * Fixed session verification when token may have the `--` separator

## v1.1.0

* Enhancements
  * Only log errors if the exception has 5xx status code
  * Warn when rendering non 5xx status code in Plug.Debugger
  * Use URL safe variant on crypto (old tokens are still valid but new ones will be generated)
  * Allow custom content-type when passing a map body in `Plug.Test`

## v1.0.3

* Enhancements
  * Raise if new lines are used in header values

* Bug fix
  * Allow mime type lookup of uppercase extensions
  * Do not validate uppercase headers in production to avoid performance hits
  * Prevent Plug.Parsers from clobbering existing conn.params when part of it is unfetched

## v1.0.2

* Bug fix
  * Ensure cookie store returns a Session ID so they can be dropped

## v1.0.1

* Enhancements
  * Allow configuring all options supported by the underlying transport (i.e. cowboy)

## v1.0.0

* Enhancements
  * Allow custom headers in `Plug.Static`

* Bug fix
  * No longer automatically assume "priv" for cert and key files for Cowboy SSL
  * Raise if response has been sent more than once in test connection
  * Raise when body is nil on `Plug.Conn.resp/3`
  * Show more info and escape messages in `Plug.Debugger`

## v0.14.0

* Enhancements
  * Support `:rewrite_on` on `Plug.SSL`
  * Add `Plug.Conn.merge_resp_headers/2`

* Bug fix
  * Ensure message encryptor and verifier do not error on bad data

## v0.13.1

* Enhancements
  * Add `conn.request_path`
  * Raise if `put_session/3` is invoked when response is sent

* Bug fixes
  * Fix empty params being encoded into query string as '&'

* Deprecations
  * `Plug.Conn.full_path/2` is deprecated in favor of `conn.request_path`
  * `Plug.Test.put_req_header/3` and `Plug.Test.delete_req_header/3` is deprecated in favor of similarly named functions in `Plug.Conn`

## v0.13.0

* Enhancements
  * Raise if a header in upcase is given
  * Store timestamps in sessions ETS table and document each entry format
  * Allow private options when specifying routes in `Plug.Router`
  * Allow the session to be cleared and ignored when an invalid CSRF token is given
  * Allow log level to be configured in `Plug.Logger`
  * Generate masked CSRF tokens to avoid BREACH attacks

* Backwards incompatible changes
  * `Plug.Logger` no longer sets the request id. Use `Plug.RequestId` instead

## v0.12.2

* Enhancements
  * Add `Plug.HTML`

* Bug fixes
  * Do not crash on poorly encoded cookies
  * Decode parameters before matching on the router

## v0.12.1

* Enhancements
  * Add `Plug.SSL` with redirection from HTTP and HSTS support
  * Remove the need for `:encrypt` option from `Plug.Session.COOKIE`. The need for encryption can be fully specified by passing `:encrypted_salt` option. This improvement is backwards compatible.

* Bug fixes
  * Ensure we don't parse body params if they were already parsed

## v0.12.0

* Enhancements
  * Add `query_params` and `body_params` to keep query and body parameters apart from `params`
  * Allow custom encoders when encoding query parameters
  * Assert valid utf-8 on url encoded and multipart bodies

* Bug fixes
  * Use only body parameters when detecting method override
  * Add Vary header when serving gzipped content in Plug.Static

* Deprecations
  * `fetch_params/2` is deprecated in favor of `fetch_query_params/2`

## v0.11.3

* Bug fixes
  * Ensure test adapter reuses the given connection

* Deprecations
  * The `:headers` option in `Plug.Test.conn/4` is deprecated in favor of `put_req_header/3`

## v0.11.2

* Enhancements
  * Add `:log_on_halt` option to `Plug.Builder` and `Plug.Router`
  * Use raw files and delayed writes on upload

* Bug fixes
  * Do not read the whole request body at once
  * Improve performance of url encoded params

* Deprecations
  * `Plug.Builder.compile/1` and `Plug.Builder.compile/2` are deprecated in favor of explicit `Plug.Builder.compile/3`

## v0.11.1

* Enhancements
  * Allow Plug mimes to be configured via application environment
  * Extend JSON parser to be compatible with all json compatible content types. This includes types with suffix `+json`
  * Add `Plug.Conn.clear_session/1`

* Bug fixes
  * Do not require cowboy at compile time
  * Also parse request bodies on DELETE requests

## v0.11.0

* Enhancements
  * Add `Plug.Conn.async_assign/3` and `Plug.Conn.await_assign/3` to start and await for assigns asynchronously, mimic'ing `Task.async/1` and `Task.await/2` behaviour
  * Add `Plug.Conn.WrapperError` to propagate an error with the connection for better debugging by either `Plug.Debugger` or `Plug.ErrorHandler`
  * Add `Plug.Conn.update_resp_header/4` to update a response header or set its initial value if not present

* Bug fixes
  * Skip parsing of files when no filename is sent
  * Fix how script_name are accumulated with multiple calls to `Plug.Router.forward/2`

* Backwards incompatible changes
  * `Plug.CSRFProtection` now uses a session to store tokens. Tokens are now generated on demand and can be accessed via `Plug.CSRFProtection.get_csrf_token/0`

## v0.10.0

* Enhancements
  * Add `:only` option to `Plug.Static` to avoid all requests triggering file system queries
  * Add ETag management to `Plug.Static` when requests to not contain a versioned query string
  * Enforce atom or string keys in `Plug.Conn.put_session/3` and friends and normalize keys to strings

* Bug fixes
  * Add UTF-8 tag to debugger templates

* Backwards incompatible changes
  * `Plug.CSRFProtection` now uses a cookie instead of session and expects a `"_csrf_token"` parameter instead of `"csrf_token"`

## v0.9.0

* Enhancements
  * Add `Plug.Conn.full_path/1`
  * Add `Plug.CSRFProtection` that adds cross-forgery protection
  * Add `Plug.ErrorHandler` that allows an error page to be sent on crashes (instead of a blank one)
  * Support host option in `Plug.Router`

* Backwards incompatible changes
  * Add `Plug.Router.Utils.build_match/1` was renamed to `build_path_match/1`

## v0.8.4

* Bug fixes
  * Clean up `{:plug_conn, :sent}` messages from listener inbox and ensure connection works accross processes

* Deprecations
  * Deprecate `recycle/2` in favor of `recycle_cookies` in Plug.Test

## v0.8.3

* Enhancements
  * Use PKCS7 padding in MessageEncryptor (the same as OpenSSL)
  * Add support for custom serializers in cookie session store
  * Allow customization of key generation in cookie session store
  * Automatically import `Plug.Conn` in Plug builder
  * Render errors from Plug when using Ranch/Cowboy nicely
  * Provide `Plug.Crypto.secure_compare/2` for comparing binaries
  * Add `Plug.Debugger` for helpful pages whenever there is a failure during a request

* Deprecations
  * Deprecate `:accept` in favor of `:pass` in Plug.Parsers

## v0.8.2

* Enhancements
  * Add `Plug.Conn.Utils.media_type/1` to provide media type parsing with wildcard support
  * Do not print adapter data by default when inspecting the connection
  * Allow plug_status to simplify the definition of plug aware exceptions
  * Allow cache headers in `Plug.Static` to be turned off

* Bug fix
  * Support dots on header parameter parsing

## v0.8.1

* Enhancements
  * Add a `Plug.Parsers.JSON` that expects a JSON decoder as argument

* Bug fix
  * Properly populate `params` field for test connections
  * Fix `Plug.Logger` not reporting the proper path

## v0.8.0

* Enhancements
  * Add `fetch_session/2`, `fetch_params/2` and `fetch_cookies/2` so they can be pluggable
  * Raise an error message on invalid router indentifiers
  * Add `put_status/2` and support atom status codes
  * Add `secret_key_base` field to the connection

* Backwards incompatible changes
  * Add `encryption_salt` and `signing_salt` to the CookieStore and derive actual keys from `secret_key_base`

## v0.7.0

* Enhancements
  * Support Elixir 1.0.0-rc1
  * Support haltable pipelines with `Plug.Conn.halt/2`
  * Ensure both Plug.Builder and Plug.Router's `call/2` are overridable

* Bug fix
  * Properly report times in Logger

* Backwards incompatible changes
  * Remove support for Plug wrappers

## v0.6.0

* Enhancements
  * Add `Plug.Logger`
  * Add `conn.peer` and `conn.remote_ip`
  * Add `Plug.Conn.sendfile/5`
  * Allow `call/2` from `Plug.Builder` to be overridable

## v0.5.3

* Enhancements
  * Update to Cowboy v1.0.0
  * Update mime types list
  * Update to Elixir v0.15.0

## v0.5.2

* Enhancements
  * Update to Elixir v0.14.3
  * Cowboy adapter now returns `{:error,:eaddrinuse}` when port is already in use
  * Add `Plug.Test.recycle/2` that copies relevant data between connections for future requests

## v0.5.1

* Enhancements
  * Add ability to configure when `Plug.Parsers` raises `UnsupportedMediaTypeError`
  * Add `Plug.Conn.Query.encode/1`
  * Add `CookieStore` for session

* Bug fixes
  * Ensure plug parses content-type with CRLF as LWS

## v0.5.0

* Enhancements
  * Update to Elixir v0.14.0
  * Update Cowboy adapter to v0.10.0
  * Add `Plug.Conn.read_body/2`

* Backwards incompatible changes
  * `Plug.Parsers` now expect `:length` instead of `:limit` and also accept `:read_length` and `:read_timeout`

## v0.4.4

* Enhancements
  * Update to Elixir v0.13.3

## v0.4.3

* Enhancements
  * Update to Elixir v0.13.2

## v0.4.2

* Enhancements
  * Update to Elixir v0.13.1

## v0.4.1

* Enhancements
  * Remove `:mime` dependency in favor of `Plug.MIME`
  * Improve errors when Cowboy is not available
  * First hex package release

## v0.4.0

* Enhancements
  * Support `before_send/1` callbacks
  * Add `Plug.Static`
  * Allow iodata as the body
  * Do not allow response headers to be set if the response was already sent
  * Add `Plug.Conn.private` to be used as storage by libraries/frameworks
  * Add `get_req_header` and `get_resp_header` for fetching request and response headers

* Backwards incompatible changes
  * `Plug.Connection` was removed in favor of `Plug.Conn`
  * `Plug.Conn` is now a struct
  * assigns, cookies, params and sessions have been converted to maps

## v0.3.0

* Definition of the Plug specification

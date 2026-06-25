"""the-benchmarker/web-frameworks server for mq-bridge-py.

Implements the three required routes on ``0.0.0.0:3000``:

==========  ============  =================
Method      Path          Response
==========  ============  =================
GET         ``/``         200, empty body
GET         ``/user/:id`` 200, the ``id``
POST        ``/user``     200, empty body
==========  ============  =================

A single ``http -> response`` route (no path/method filter) reaches the handler,
which dispatches on the request's ``http_method`` / ``http_path`` metadata and
extracts ``:id`` as the suffix after ``/user/``. The inline-response fast path
keeps all HTTP framing in Rust (off the GIL); the Python handler runs only the
trivial dispatch.
"""

from __future__ import annotations

import os
import tempfile

from mq_bridge import Message, Route

LISTEN = os.environ.get("MQB_LISTEN", "0.0.0.0:3000")
USER_PREFIX = "/user/"

CONFIG = f"""
routes:
  the-benchmarker:
    concurrency: 1
    batch_size: 512
    input:
      http:
        url: "{LISTEN}"
        concurrency_limit: 65536
        internal_buffer_size: 16384
        inline_response_fast_path: true
    output:
      response: {{}}
"""

TEXT_META = {"content-type": "text/plain"}
NOT_FOUND_META = {"content-type": "text/plain", "http_status_code": "404"}


def handle(message: Message) -> Message:
    method = message.metadata.get("http_method", "")
    path = message.metadata.get("http_path", "")

    if method == "GET" and path == "/":
        return Message(b"")
    if method == "POST" and path == "/user":
        return Message(b"")
    if method == "GET" and path.startswith(USER_PREFIX):
        user_id = path[len(USER_PREFIX) :]
        if user_id and "/" not in user_id:
            return Message(user_id.encode(), TEXT_META)
    return Message(b"Not Found", NOT_FOUND_META)


def main() -> None:
    with tempfile.NamedTemporaryFile("w", suffix=".yaml", delete=False) as handle_file:
        handle_file.write(CONFIG)
        config_path = handle_file.name
    route = Route.from_yaml(config_path, "the-benchmarker").with_handler(handle)
    route.run()


if __name__ == "__main__":
    main()

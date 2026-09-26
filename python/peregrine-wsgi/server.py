_PLAIN_TEXT = [("Content-Type", "text/plain; charset=utf-8")]


def app(environ, start_response):
    path = environ["PATH_INFO"]
    method = environ["REQUEST_METHOD"]

    if method == "GET":
        if path == "/":
            start_response("200 OK", [])
            return [b""]

        if path.startswith("/user/"):
            start_response("200 OK", _PLAIN_TEXT)
            return [path[6:].encode()]

    elif method == "POST" and path == "/user":
        start_response("200 OK", [])
        return [b""]

    start_response("404 Not Found", [])
    return [b""]

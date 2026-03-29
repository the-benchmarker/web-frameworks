import fastpysgi


async def app(scope, receive, send):
    if scope["type"] != "http":
        return

    path = scope["path"]
    req_method = scope.get("method", "")
    headers = []

    if req_method == "GET":
        if path == "/":
            await send({"type": "http.response.start", "status": 200, "headers": []})
            await send({"type": "http.response.body", "body": b"", "more_body": False})
            return

        if path.startswith("/user/"):
            value = path[6:]
            await send(
                {
                    "type": "http.response.start",
                    "status": 200,
                    "headers": [[b"Content-Type", b"text/plain; charset=utf-8"]],
                }
            )
            await send(
                {
                    "type": "http.response.body",
                    "body": value.encode(),
                    "more_body": False,
                }
            )
            return

    if req_method == "POST":
        if path == "/user":
            await send({"type": "http.response.start", "status": 200, "headers": []})
            await send({"type": "http.response.body", "body": b"", "more_body": False})
            return

    await send({"type": "http.response.start", "status": 404, "headers": []})
    await send({"type": "http.response.body", "body": b"", "more_body": False})


if __name__ == "__main__":
    import optparse

    parser = optparse.OptionParser("usage: %prog [options]", add_help_option=False)
    parser.add_option("-h", "--host", dest="host", default="0.0.0.0", type="string")
    parser.add_option("-p", "--port", dest="port", default=3000, type="int")
    parser.add_option("-w", "--workers", dest="workers", default=0, type="int")
    (opt, args) = parser.parse_args()

    workers = opt.workers
    if workers <= 0:
        import multiprocessing

        workers = multiprocessing.cpu_count()

    fastpysgi.run(app, opt.host, opt.port, workers=workers)

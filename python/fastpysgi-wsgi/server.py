import fastpysgi


def app(env, start_response):
    path = env["PATH_INFO"]
    req_method = env.get("REQUEST_METHOD", "")
    headers = []

    if req_method == "GET":
        if path == "/":
            start_response("200 OK", headers)
            return [b""]

        if path.startswith("/user/"):
            value = path[6:]
            start_response("200 OK", [("Content-Type", "text/plain; charset=utf-8")])
            return [value.encode()]

    if req_method == "POST":
        if path == "/user":
            start_response("200 OK", headers)
            return [b""]

    start_response("404 Not Found", headers)
    return [b""]


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

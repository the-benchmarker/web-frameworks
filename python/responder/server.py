import responder

app = responder.API()


@app.route("/")
async def index(req, resp):
    resp.headers["Content-Type"] = "text/plain"
    resp.text = f""


@app.route("/user/{id:int}")
async def user_info(req, resp, *, id):
    resp.headers["Content-Type"] = "text/plain"
    resp.text = f"{id}"


@app.route("/user")
async def user(req, resp):
    if req.method == "post":
        resp.headers["Content-Type"] = "text/plain"
        resp.text = f""

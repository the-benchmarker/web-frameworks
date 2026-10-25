from wred import App

app = App()


@app.route("/")
async def index(request):
    return b"", [("content-type", "text/plain")], 200


@app.route("/user/{id}")
async def get_user(request, id):
    return id.encode(), [("content-type", "text/plain")], 200


@app.route("/user", methods=["POST"])
async def create_user(request):
    return b"", [("content-type", "text/plain")], 200

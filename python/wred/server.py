from wred import App

app = App()


@app.route("/")
async def index(request):
    return b"", "text/plain", 200


@app.route("/user/{id}")
async def get_user(request, id):
    return id.encode(), "text/plain", 200


@app.route("/user", methods=["POST"])
async def create_user(request):
    return b"", "text/plain", 200

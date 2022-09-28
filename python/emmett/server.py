from emmett import App

app = App(__name__)
app.config.handle_static = False


@app.route("/", output="bytes")
async def index():
    return b""


@app.route("/user/<any:id>", output="str")
async def user_info(id):
    return id


@app.route("/user", methods="post", output="bytes")
async def user():
    return b""

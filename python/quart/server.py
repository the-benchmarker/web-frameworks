from quart import Quart

app = Quart(__name__)


@app.route("/")
async def index():
    return ""


@app.route("/user/<id>")
async def user_info(id):
    return id


@app.route("/user", methods=["POST"])
async def user():
    return ""

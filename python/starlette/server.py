from starlette.applications import Starlette
from starlette.responses import PlainTextResponse

app = Starlette()


@app.route("/")
async def homepage(request):
    return PlainTextResponse("")


@app.route("/user/{user_id}")
async def user(request):
    user_id = request.path_params["user_id"]
    return PlainTextResponse(user_id)


@app.route("/user", methods=["POST"])
async def userinfo(request):
    return PlainTextResponse("")

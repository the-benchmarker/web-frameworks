from sanic import Sanic
from sanic.response import text
import sys

app = Sanic(log_config=None)
WORKERS = int(sys.argv[-1])


@app.route("/")
async def index(request):
    return text("")


@app.route("/user/<id:int>", methods=["GET"])
async def user_info(request, id):
    return text(str(id))


@app.route("/user", methods=["POST"])
async def user(request):
    return text("")


if __name__ == "__main__":
    app.run(
        debug=False,
        access_log=False,
        workers=WORKERS,
        port=3000,
        host="0.0.0.0",
    )

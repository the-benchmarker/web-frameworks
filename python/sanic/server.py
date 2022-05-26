import multiprocessing

from sanic import Sanic
from sanic.response import text


app = Sanic("benchmark")


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
    workers = multiprocessing.cpu_count()
    app.run(host="0.0.0.0", port=3000, workers=workers, debug=False, access_log=False)

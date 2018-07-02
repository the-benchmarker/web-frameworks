from sanic import Sanic
from sanic.response import text
from signal import signal, SIGINT
import asyncio
import uvloop

app = Sanic(log_config=None)


@app.route("/")
async def index(request):
    return text('')


@app.route("/user/<id:int>", methods=['GET'])
async def user_info(request, id):
    return text(str(id))


@app.route("/user", methods=['POST'])
async def user(request):
    return text('')

if __name__ == "__main__":
    asyncio.set_event_loop(uvloop.new_event_loop())
    server = app.create_server(host="0.0.0.0", port=3000)
    loop = asyncio.get_event_loop()
    task = asyncio.ensure_future(server)
    signal(SIGINT, lambda s, f: loop.stop())
    try:
        loop.run_forever()
    except:
        loop.stop()

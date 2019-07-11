from blacksheep.server import Application,ServerOptions
import multiprocessing
from blacksheep.server.responses import text


app = Application(ServerOptions( host = '0.0.0.0' , port = 3000 , processes_count = multiprocessing.cpu_count() ), debug = False)

@app.router.get('/')
async def home(_):
    return text(f"")

@app.router.get("/user/:id")
async def user_info(_, id : int):
    return text(f"{id}")


@app.router.post("/user")
async def user(_):
    return text(f"")

app.start()

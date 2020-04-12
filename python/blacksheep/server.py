from blacksheep.server import Application
from blacksheep.server.responses import text


app = Application()


@app.router.get("/")
async def home(_):
    return text(f"")


@app.router.get("/user/:id")
async def user_info(_, id: int):
    return text(f"{id}")


@app.router.post("/user")
async def user(_):
    return text(f"")


app.start()

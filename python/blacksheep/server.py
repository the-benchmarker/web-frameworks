from blacksheep.server import Application
from blacksheep.server.responses import text


app = Application()


@app.router.get("/")
async def home(_):
    return text(f"")


@app.router.get("/user/:user_id")
async def user_info(_, user_id: int):
    return text(f"{user_id}")


@app.router.post("/user")
async def user(_):
    return text(f"")


app.start()

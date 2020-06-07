from fastapi import FastAPI
from starlette.responses import PlainTextResponse

app = FastAPI()


@app.get("/")
async def index():
    return PlainTextResponse(content="")


@app.get("/user/{user_id}")
async def get_user(user_id: int):
    return PlainTextResponse(content=f"{user_id}".encode())


@app.post("/user")
async def create_user():
    return PlainTextResponse(content="")

from fastapi import FastAPI
from starlette.responses import PlainTextResponse

app = FastAPI()


@app.get("/")
async def index():
    return PlainTextResponse(content="")


@app.get("/user/{id}")
async def get_user(id: int):
    return PlainTextResponse(content=f"{id}".encode())


@app.post("/user")
async def create_user():
    return PlainTextResponse(content="")

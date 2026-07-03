from fastapi import APIRouter
from starlette.responses import PlainTextResponse

public = APIRouter()


@public.get("/")
async def index():
    return PlainTextResponse(content="")


@public.get("/user/{id}")
async def get_user(id: int):
    return PlainTextResponse(content=f"{id}".encode())


@public.post("/user")
async def create_user():
    return PlainTextResponse(content="")

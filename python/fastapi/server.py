from fastapi import FastAPI, APIRouter
from starlette.responses import PlainTextResponse

router = APIRouter()


@router.get("/")
async def index():
    return PlainTextResponse(content="")


@router.get("/user/{id}")
async def get_user(id: int):
    return PlainTextResponse(content=f"{id}".encode())


@router.post("/user")
async def create_user():
    return PlainTextResponse(content="")


app = FastAPI()
app.include_router(router)

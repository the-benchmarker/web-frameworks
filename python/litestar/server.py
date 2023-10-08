from litestar import Litestar, MediaType, get, post


@get("/", media_type=MediaType.TEXT)
async def index() -> str:
    return ""


@get("/user/{id:int}", media_type=MediaType.TEXT)
async def get_user(id: int) -> str:
    return f"{id}".encode()


@post("/user", media_type=MediaType.TEXT)
async def create_user() -> str:
    return ""


app = Litestar(
    route_handlers=[index, get_user, create_user],
    openapi_config=None,
)

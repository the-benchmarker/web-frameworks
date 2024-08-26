from panther import Panther
from panther.app import API
from panther.response import PlainTextResponse


@API(methods=["GET"])
async def index():
    return PlainTextResponse("")


@API(methods=["GET"])
async def get_user(id: str):
    return PlainTextResponse(id)


@API(methods=["POST"])
async def create_user() -> str:
    return PlainTextResponse("")


urls = {"/": index, "/user/<id>": get_user, "/user": create_user}

app = Panther(__name__, configs=__name__, urls=urls)

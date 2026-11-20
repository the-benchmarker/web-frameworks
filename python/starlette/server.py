from starlette.applications import Starlette
from starlette.responses import PlainTextResponse
from starlette.routing import Route

app = Starlette()


async def homepage(request):
    return PlainTextResponse("")


async def user(request):
    user_id = request.path_params["user_id"]
    return PlainTextResponse(user_id)


async def userinfo(request):
    return PlainTextResponse("")


app = Starlette(
    routes=[
        Route("/", homepage),
        Route("/user/{user_id}", user),
        Route("/user", userinfo, methods=["POST"]),
    ]
)

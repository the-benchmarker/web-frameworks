from baize.asgi import request_response, Router, PlainTextResponse


@request_response
async def homepage(request):
    return PlainTextResponse("")


@request_response
async def user(request):
    return PlainTextResponse(request.path_params["user_id"])


@request_response
async def userinfo(request):
    return PlainTextResponse("")


app = Router(
    ("/", homepage),
    ("/user/{user_id}", user),
    ("/user", userinfo),
)

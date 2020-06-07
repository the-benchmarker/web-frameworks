from aiohttp import web

routes = web.RouteTableDef()


@routes.get("/")
async def index(request):
    return web.Response(text="")


@routes.post("/user")
async def user_info(request):
    return web.Response(text="")


@routes.get("/user/{user_id}")
async def user_id(request):
    return web.Response(text=request.match_info["user_id"])


async def app():
    app = web.Application()
    app.add_routes(routes)
    return app

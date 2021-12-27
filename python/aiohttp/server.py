# Disable all logging features
import logging

logging.disable()


from aiohttp import web

routes = web.RouteTableDef()


@routes.get("/")
async def index(request):
    return web.Response(text="")


@routes.post("/user")
async def user_info(request):
    return web.Response(text="")


@routes.get("/user/{id}")
async def user_id(request):
    return web.Response(text=request.match_info["id"])


async def app():
    app = web.Application()
    app.add_routes(routes)
    return app

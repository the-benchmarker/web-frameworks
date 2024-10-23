from guillotina import configure
from guillotina.factory import make_app
from guillotina.interfaces import IApplication
from guillotina.response import Response


@configure.service(method="GET", context=IApplication, permission="guillotina.Public")
async def index(context, request):
    return Response(body=b"")


@configure.service(
    method="GET",
    context=IApplication,
    permission="guillotina.Public",
    name="/user/{id}",
)
async def user_info(context, request):
    id_ = request.matchdict["id"]
    return Response(body=id_.encode("utf-8"))


@configure.service(method="POST", context=IApplication, permission="guillotina.Public", name="/user")
async def user_info(context, request):
    return Response(body=b"")


app = make_app(settings={"applications": ["server"]})

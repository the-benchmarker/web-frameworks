from lihil import Lihil, Route
from lihil.vendors import Response

root = Route()
user_route = root / "user"


@root.get
async def homepage():
    return Response(media_type="text/plain")


@user_route.post
async def userinfo():
    return Response(media_type="text/plain")


@user_route.sub("/{user_id}").get
async def get_user(user_id: str):
    return Response(content=user_id.encode(), media_type="text/plain")


app = Lihil(root, user_route)

from lihil import Lihil, Route, Text
from lihil.vendors import Response

all_users = Route("/user")
user = all_users / "{user_id}"


@user.get
async def get_user(user_id: str) -> Text:
    return Response(content=user_id.encode(), media_type="text/plain")


@all_users.post
async def userinfo() -> Text:
    return Response(media_type="text/plain")


app = Lihil(user, all_users)


@app.get
async def homepage() -> Text:
    return Response(media_type="text/plain")

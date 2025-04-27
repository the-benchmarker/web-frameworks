from lihil import Lihil, Route, Text

all_users = Route("/user")
user = all_users / "{user_id}"


@user.get
async def get_user(user_id: str) -> Text:
    return user_id


@all_users.post
async def userinfo() -> Text:
    return ""


lhl = Lihil[None](routes=[user, all_users])


@lhl.get
async def homepage() -> Text:
    return ""

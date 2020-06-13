import uvicorn

from tonberry import create_app, expose
from tonberry.content_types import TextPlain


class User:
    @expose.get
    async def index(self, user_id: int) -> TextPlain:
        return f"{user_id}"


class Root:
    @expose.get
    async def index(self) -> TextPlain:
        return ""

    @expose.post
    async def user(self) -> TextPlain:
        return ""


if __name__ == "__main__":
    app = create_app(root=Root)
    uvicorn.run(app, host="127.0.0.1", port=8888)

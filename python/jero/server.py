from jero import BaseApp, Endpoint, Struct


class UserPath(Struct):
    id: str


class Index(Endpoint, path="/"):
    async def get(self) -> str:
        return ""


class GetUser(Endpoint, path="/user/{id}"):
    async def get(self, path: UserPath) -> str:
        return path.id


class CreateUser(Endpoint, path="/user"):
    async def post(self) -> str:
        return ""


class App(BaseApp):
    async def wire(self) -> None:
        self._include_endpoint(Index())
        self._include_endpoint(GetUser())
        self._include_endpoint(CreateUser())


app = App()

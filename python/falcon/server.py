import falcon
from meinheld import patch

patch.patch_all()


class EmptyResponse:
    def on_get(self, req, resp):
        resp.data = "".encode()

    def on_post(self, req, resp):
        resp.data = "".encode()


class StringResponse:
    def on_get(self, req, resp, id: int):
        resp.data = f"{id}".encode()


app = falcon.API()
app.add_route("/", EmptyResponse())
app.add_route("/user", EmptyResponse())
app.add_route("/user/{id:int}", StringResponse())

import falcon
from meinheld import patch

patch.patch_all()


class EmptyResponse:
    def on_get(self, req, resp):
        resp.data = bytes("", "utf8")

    def on_post(self, req, resp):
        resp.data = bytes("", "utf8")


class StringResponse:
    def on_get(self, req, resp, id):
        resp.data = bytes(id, "utf8")


app = falcon.API()
app.add_route("/", EmptyResponse())
app.add_route("/user", EmptyResponse())
app.add_route("/user/{id}", StringResponse())

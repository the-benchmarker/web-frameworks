import falcon
from meinheld import patch

patch.patch_all()


class EmptyResponse:
    @staticmethod
    def on_get(req, resp):
        resp.data = bytes("", "utf8")

    @staticmethod
    def on_post(req, resp):
        resp.data = bytes("", "utf8")


class StringResponse:
    @staticmethod
    def on_get(req, resp, id):
        resp.data = bytes(id, "utf8")


app = falcon.API()
app.add_route("/", EmptyResponse())
app.add_route("/user", EmptyResponse())
app.add_route("/user/{id}", StringResponse())

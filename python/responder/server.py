import responder
from marshmallow import fields


class EmptyString(fields.Field):
    def encode(field, options):
        return ''


app = responder.API()


@app.route("/")
async def index(req, resp):
    resp.text = EmptyString()


@app.route('/user/{id}')
async def user_info(req, resp, *, id):
    resp.text = id


@app.route('/user')
async def user(req, resp):
    if req.method == 'post':
        resp.text = EmptyString()

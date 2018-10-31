import responder

api = responder.API()


@api.route("/")
async def index(req, resp):
    resp.text = ""


@api.route('/user/{id}')
async def user_info(req, resp, *, id):
    resp.text = id


@api.route('/user')
async def user(req, resp):
    if req.method == 'post':
        resp.text = ''

if __name__ == "__main__":
    api.run(port=3000)

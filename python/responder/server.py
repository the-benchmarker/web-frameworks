import responder

app = responder.API()


@app.route("/")
async def index(req, resp):
    resp.text = ""


@app.route('/user/{id}')
async def user_info(req, resp, *, id):
    resp.text = id


@app.route('/user')
async def user(req, resp):
    if req.method == 'post':
        resp.text = ''

if __name__ == "__main__":
    api.run(port=3000)

from bocadillo import App,view

app = App()


@app.route("/")
async def index(req, res):
    res.text = ""


@app.route("/user")
@view(methods=["post"])
async def greet(req, res):
    res.text = ""


@app.route("/user/{id}")
async def user_info(req, res, id):
    res.text = id

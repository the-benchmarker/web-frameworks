from bocadillo import App, configure

app = App()
configure(app)


@app.route("/")
async def index(req, res):
    res.text = ""


@app.route("/user", methods=["post"])
async def create_user(req, res):
    res.text = ""


@app.route("/user/{pk}")
async def get_user(req, res, pk: int):
    res.text = pk

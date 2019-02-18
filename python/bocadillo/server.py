import bocadillo

app = bocadillo.API()


@app.route("/")
async def index(req, res):
    res.text = ""


@app.route("/user")
@bocadillo.view(methods=["post"])
async def greet(req, res):
    res.text = ""


@app.route("/user/{id}")
async def user_info(req, res, id):
    res.text = id

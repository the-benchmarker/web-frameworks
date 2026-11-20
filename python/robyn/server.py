from robyn import Robyn

app = Robyn(__file__)


@app.get("/")
async def index(request):
    return ""


@app.get("/user/:id")
async def user_info(request):
    return request.path_params.get("id")


@app.post("/user")
async def user(request):
    return ""


app.start(host="0.0.0.0", port=3000)

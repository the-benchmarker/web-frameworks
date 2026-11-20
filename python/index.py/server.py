from indexpy import Index, request

app = Index()


@app.router.http("/")
async def homepage():
    return ""


@app.router.http("/user/{user_id}")
async def user():
    return request.path_params["user_id"]


@app.router.http("/user")
async def userinfo():
    return ""

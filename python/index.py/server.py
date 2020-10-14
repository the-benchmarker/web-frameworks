from indexpy import Index

app = Index(try_html=False)


@app.router.http("/", method="get")
async def homepage(request):
    return ""


@app.router.http("/user/{user_id}", method="get")
async def user(request):
    return request.path_params["user_id"]


@app.router.http("/user", method="post")
async def userinfo(request):
    return ""

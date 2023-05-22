import asgineer


@asgineer.to_asgi
async def app(request):
    if request.method == "GET":
        if request.path == "/":
            return ""
        if request.path.startswith("/user/"):
            return await output_second_param(request.path)
    elif request.method == "POST":
        return ""
    else:
        return 404, {}, f"404 not found {request.path}"


async def output_second_param(path):
    params = path.split("/")
    return params[2]

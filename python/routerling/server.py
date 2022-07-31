from routerling import Router, HttpRequest, ResponseWriter, Context


app = Router()


async def get_user_by_id(req: HttpRequest, res: ResponseWriter, ctx: Context):
    res.body = f"{req.params.get('id')}"


async def empty_text_response(req: HttpRequest, res: ResponseWriter, ctx: Context):
    res.body = ""


app.GET("/", empty_text_response)
app.GET("/user/:id", get_user_by_id)
app.POST("/user", empty_text_response)

from heaven import Router


async def empty_text_response(req, res, ctx):
    res.body = ""


async def get_user_by_id(req, res, ctx):
    res.body = f"{req.params.get('id')}"


app = Router()


app.GET("/", empty_text_response)
app.GET("/user/:id", get_user_by_id)
app.POST("/user", empty_text_response)

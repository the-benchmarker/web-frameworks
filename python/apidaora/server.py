from apidaora import appdaora, route


@route.get('/')
async def index() -> str:
    return ''


@route.post('/user')
async def user_info() -> str:
    return ''


@route.get('/user/{id}')
async def user_id(id: str) -> str:
    return str(id)


app = appdaora((index, user_info, user_id))

from happyx import new_server


app = new_server('0.0.0.0', 3000)


@app.get('/')
async def index():
    return ''


@app.get('/user/{id}')
async def get_user(id: int):
    return f'{id}'


@app.post('/user')
async def create_user():
    return ''

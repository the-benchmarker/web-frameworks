from quart import Quart

app = Quart(__name__)


@app.route('/')
async def index():
    return ''


@app.route('/user/<id>')
async def user_info(id):
    return id


@app.route('/user', methods=['POST'])
async def user():
    return ''

if __name__ == "__main__":
    app.run(host='0.0.0.0', port=3000)

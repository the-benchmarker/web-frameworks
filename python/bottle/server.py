from bottle import Bottle, run

app = Bottle()

@app.route('/')
def index():
    return ""


@app.route('/user/<id:int>')
def user_info(id):
    return id


@app.route('/user', method='POST')
def user():
    return ''

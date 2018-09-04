from bottle import route, run


@route('/')
def index():
    return ''


@route('/user/<id>')
def user_info(id):
    return id


@route('/user', method='POST')
def user():
    return ''

if __name__ == "__main__":
    run(host='0.0.0.0', port=3000)

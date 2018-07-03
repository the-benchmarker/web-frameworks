from vibora import Vibora, Response

app = Vibora()

@app.route('/')
async def index():
    return Response(b'', headers={'content-type': 'html'})

@app.route("/user/<id>", methods=['GET'])
async def user_info(id : int):
    return Response(f'{id}', headers={'content-type': 'html'})


@app.route("/user", methods=['POST'])
async def user():
    return Response(b'', headers={'content-type': 'html'})

if __name__ == '__main__':
    app.run(debug=True, host='0.0.0.0', port=3000)

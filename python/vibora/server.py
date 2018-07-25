from vibora import Vibora, Response

app = Vibora()

@app.route('/')
async def index():
    return Response(b'')

@app.route("/user/<id>", methods=['GET'])
async def user_info(id : int):
    return Response(f'{id}'.encode())

@app.route("/user", methods=['POST'])
async def user():
    return Response(b'')

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=3000, debug=False)

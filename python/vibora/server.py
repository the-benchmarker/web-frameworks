from vibora import Vibora, Response
import multiprocessing

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
    app.run(host='0.0.0.0', port=3000, workers=multiprocessing.cpu_count(), debug=False)

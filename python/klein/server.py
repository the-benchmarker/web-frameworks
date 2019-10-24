from klein import Klein
app = Klein()

@app.route('/')
def index(request):
    return ""

@app.route("/user/<id>", methods=["GET"])
def user_info(request):
  return request.args.get('id')


@app.route("/user", methods=["POST"])
def user(request):
    return ""

application = app.resource

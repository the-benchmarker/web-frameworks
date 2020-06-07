from klein import Klein

app = Klein()


@app.route("/")
def index(request):
    return ""


@app.route("/user/<int:user_id>", methods=["GET"])
def user_info(request, user_id):
    return str(id)


@app.route("/user", methods=["POST"])
def user(request):
    return ""


application = app.resource

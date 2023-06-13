from bottle import Bottle


app = Bottle()


@app.route("/")
def index():
    return ""


@app.route("/user/<id:int>")
def user_info(id):
    return str(id)


@app.route("/user", method="POST")
def user():
    return ""


if __name__ == "__main__":
    import sys

    options = {"host": "0.0.0.0", "port": sys.argv[1]}
    if len(sys.argv) > 2:
        options["server"] = sys.argv[2]
    app.run(**options)

from flask import Flask


app = Flask(__name__)


@app.route("/")
def index():
    return ""


@app.route("/user/<int:id>", methods=["GET"])
def user_info(id):
    return str(id)


@app.route("/user", methods=["POST"])
def user():
    return ""

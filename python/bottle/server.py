# Disable all logging features
import logging

logging.disable()
from meinheld import patch

patch.patch_all()

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

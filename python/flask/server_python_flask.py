#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from flask import Flask

import logging

log = logging.getLogger('werkzeug')
log.setLevel(logging.ERROR)

app = Flask(__name__)


@app.route("/")
def index():
    return ''


@app.route("/user/<int:id>", methods=['GET'])
def user_info(id):
    return str(id)


@app.route("/user", methods=['POST'])
def user():
    return ''


if __name__ == '__main__':
    app.run(port=3000)

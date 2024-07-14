"""A WelcomeController Module."""

from masonite.views import View
from masonite.request import Request
from masonite.controllers import Controller


class UserController(Controller):
    """WelcomeController Controller Class."""

    def index(self):
        return ""

    def show(self, request: Request):
        return request.param("id")

    def create(self):
        return ""

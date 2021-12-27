"""A WelcomeController Module."""
from masonite.views import View
from masonite.request import Request
from masonite.controllers import Controller


class UserController(Controller):
    """WelcomeController Controller Class."""

    def index(self, view: View):
        return view.render("")

    def show(self, request: Request):
        return view.render(request.input('id'))

    def create(self, view: View):
        return view.render("")
from masonite.controllers import Controller
from masonite.views import View


class UserController(Controller):
    def index(self, view: View):
        return view.render("")

    def show(self, request: Request):
        return view.render(request.input('id'))

    def create(self, view: View):
        return view.render("")
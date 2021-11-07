from masonite.request import Request
from masonite.controllers import Controller


class PageController(Controller):
    def index(self):
        return ""

    def show_user(self, request: Request):
        return request.param("id")

    def create_user(self):
        return ""

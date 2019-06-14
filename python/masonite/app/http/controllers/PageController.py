from masonite.view import View
from masonite.request import Request


class PageController:
    def index(self, request: Request):
        return ""

    def show_user(self, request: Request):
        return request.param("id")

    def create_user(self):
        return ""

from masonite.request import Request


class PageController:
    @staticmethod
    def index(request: Request):
        return ""

    @staticmethod
    def show_user(request: Request):
        return request.param("id")

    @staticmethod
    def create_user():
        return ""

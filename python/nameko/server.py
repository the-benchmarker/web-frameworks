from nameko.web.handlers import http


class HttpService:
    name = "http_service"

    @http("GET", "/")
    @staticmethod
    def index(request):
        return ""

    @http("GET", "/user/<int:id>")
    @staticmethod
    def get_user(request, id):
        return str(id)

    @http("POST", "/user")
    @staticmethod
    def do_post(request):
        return ""

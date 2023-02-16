from nameko.web.handlers import http


class HttpService:
    name = "http_service"

    @http("GET", "/")
    def index(self, request):
        return ""

    @http("GET", "/user/<int:id>")
    def get_user(self, request, id):
        return str(id)

    @http("POST", "/user")
    def do_post(self, request):
        return ""

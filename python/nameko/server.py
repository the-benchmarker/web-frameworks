from nameko.web.handlers import http


class HttpService:
    name = "http_service"

    @http("GET", "/")
    def index(request):
        return ""

    @http("GET", "/user/<int:id>")
    def get_user(request, id):
        return str(id)

    @http("POST", "/user")
    def do_post(request):
        return ""

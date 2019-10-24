import cyclone.web
from twisted.application import internet
from twisted.application import service


class MainHandler(cyclone.web.RequestHandler):
    @cyclone.web.asynchronous
    def get(self):
        self.write("")
        self.finish()


class UserInfoHandler(cyclone.web.RequestHandler):
    @cyclone.web.asynchronous
    def get(self, id):
        self.write(id)
        self.finish()


class UserHandler(cyclone.web.RequestHandler):
    @cyclone.web.asynchronous
    def post(self):
        self.write("")
        self.finish()


routes = cyclone.web.Application(
    [(r"/", MainHandler), (r"/user/(\d+)", UserInfoHandler), (r"/user", UserHandler)]
)
application = service.Application("benchmark")
server = internet.TCPServer(3000, routes, interface="0.0.0.0")
server.setServiceParent(application)

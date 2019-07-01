import cyclone.web
from twisted.application import internet
from twisted.application import service
from twisted.internet import reactor


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


application = cyclone.web.Application(
    [(r"/", MainHandler), (r"/user/(\d+)", UserInfoHandler), (r"/user", UserHandler)]
)
reactor.listenTCP(port=3000, interface="0.0.0.0", factory=application)
reactor.run()

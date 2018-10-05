import cyclone.web
import sys

from twisted.internet import reactor
from twisted.python import log


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


if __name__ == "__main__":
    application = cyclone.web.Application([
        (r'/', MainHandler),
        (r"/user/(\d+)", UserInfoHandler),
        (r"/user", UserHandler),
    ])

    log.startLogging(sys.stdout)
    reactor.listenTCP(port=3000, interface='0.0.0.0', factory=application)
    reactor.run()

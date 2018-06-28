from tornado.web import Application, RequestHandler, asynchronous
from tornado.ioloop import IOLoop

class MainHandler(RequestHandler):

    def get(self):
        self.write('')


class UserHandler(RequestHandler):

    def post(self):
        self.write('')


class UserInfoHandler(RequestHandler):

    def get(self, id):
        self.write(id)


app = Application([
    (r"/", MainHandler),
    (r"/user", UserHandler),
    (r"/user/(\d+)", UserInfoHandler)
])

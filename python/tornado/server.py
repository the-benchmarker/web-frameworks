import tornado.httpserver
import tornado.ioloop
import tornado.web


class MainHandler(tornado.web.RequestHandler):
    def get(self):
        self.set_status(200)


class UserHandler(tornado.web.RequestHandler):
    def post(self):
        self.set_status(200)


class UserInfoHandler(tornado.web.RequestHandler):
    def get(self, id):
        self.write(id)


app = tornado.web.Application(
    handlers=[
        (r"/", MainHandler),
        (r"/user", UserHandler),
        (r"/user/(\d+)", UserInfoHandler),
    ]
)

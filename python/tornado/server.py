import tornado.httpserver
import tornado.ioloop
import tornado.web

class MainHandler(tornado.web.RequestHandler):

    def get(self):
        self.write('')


class UserHandler(tornado.web.RequestHandler):

    def post(self):
        self.write('')


class UserInfoHandler(tornado.web.RequestHandler):

    def get(self, id):
        self.write(id)


if __name__ == '__main__':
    app = tornado.web.Application(handlers=[(r'/', MainHandler),
                                  (r"/user", UserHandler),
                                  (r"/user/(\d+)", UserInfoHandler)])
    http_server = tornado.httpserver.HTTPServer(app)
    http_server.bind(3000, address='0.0.0.0', reuse_port=True)
    http_server.start(0)
    tornado.ioloop.IOLoop.current().start()

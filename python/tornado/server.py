import tornado.httpserver
import tornado.ioloop
import tornado.web


class MainHandler(tornado.web.RequestHandler):
    def get(self):
        pass


class UserHandler(tornado.web.RequestHandler):
    def post(self):
        pass


class UserInfoHandler(tornado.web.RequestHandler):
    def get(self, id):
        self.write(id)


def main():
    app = tornado.web.Application(
        handlers=[
            (r"/", MainHandler),
            (r"/user", UserHandler),
            (r"/user/(\d+)", UserInfoHandler),
        ]
    )
    server = tornado.httpserver.HTTPServer(app)
    server.bind(3000)
    server.start(0)
    tornado.ioloop.IOLoop.current().start()


if __name__ == "__main__":
    main()

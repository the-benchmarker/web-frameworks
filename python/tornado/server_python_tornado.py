#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import tornado.httpserver
import tornado.ioloop
import tornado.options
import tornado.web

from tornado.options import define, options
define('port', default=3000, help='run on the given port', type=int)


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
    options.logging = None
    tornado.options.parse_command_line()
    app = tornado.web.Application(handlers=[(r'/', MainHandler),
                                  (r"/user", UserHandler),
                                  (r"/user/(\d+)", UserInfoHandler)])
    http_server = tornado.httpserver.HTTPServer(app)
    http_server.listen(options.port)
    tornado.ioloop.IOLoop.current().start()

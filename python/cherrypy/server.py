#! /usr/bin/env python3
import cherrypy


class WebRoot:
    @cherrypy.expose
    def index(self):
        return ""


@cherrypy.expose
class UserAPI:
    def GET(self, user_id):
        return user_id

    def POST(self):
        return ""


WebRoot.user = UserAPI()


global_config = {"environment": "production" if IS_STANDALONE else "embedded"}
config = {"/user": {"request.dispatch": cherrypy.dispatch.MethodDispatcher()}}


cherrypy.config.update(global_config)

app = cherrypy.tree.mount(WebRoot(), "", config)

if __name__ == "__main__":
    cherrypy.quickstart(app)
else:
    # on top of another WSGI server
    # https://docs.cherrypy.org/en/latest/deploy.html#uwsgi
    cherrypy.server.unsubscribe()
    cherrypy.engine.start()

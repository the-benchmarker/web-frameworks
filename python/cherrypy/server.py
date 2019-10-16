import cherrypy

class Root(object):
    @cherrypy.expose
    def index(self):
        return ""

    @cherrypy.expose
    def user(self, *args):
        if cherrypy.request.method == "POST" and len(args) == 0:
            return ""
        elif cherrypy.request.method == "GET" and len(args) == 1:
            return str(args[0])
        
        return cherrypy.HTTPError(404)

cherrypy.config.update({'engine.autoreload.on': False})

app = cherrypy.tree.mount(Root())

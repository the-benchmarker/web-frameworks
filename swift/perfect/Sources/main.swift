import PerfectHTTP
import PerfectHTTPServer

let server = HTTPServer()

var routes = Routes()

routes.add(method: .get, uri: "/", handler: { _, response in
    response.completed()
})

routes.add(method: .get, uri: "/user/{id}", handler: { request, response in
    response.setBody(string: request.urlVariables["id"]!)
    response.completed()
})

routes.add(method: .post, uri: "/user", handler: { _, response in
    response.completed()
})

server.addRoutes(routes)

server.serverPort = 3000

try! server.start()

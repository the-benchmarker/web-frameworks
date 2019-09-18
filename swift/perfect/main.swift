import PerfectHTTP
import PerfectHTTPServer

var routes = Routes()

routes.add(method: .get, uri: "/", handler: { _, response in
    response.completed()
})

routes.add(method: .get, uri: "/user/{id}", handler: { request, response in
    response.setBody(string: request.urlVariables["id"] ?? "")
    response.completed()
})

routes.add(method: .post, uri: "/user", handler: { _, response in
    response.completed()
})

try HTTPServer.launch(
    name: "0.0.0.0",
    port: 3000,
    routes: routes
)

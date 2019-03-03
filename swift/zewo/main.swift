import HTTPServer
import SwiftGlibc

let app = BasicRouter { route in
    route.get("/") { request in
        return Response(body: "")
    }

    route.get("/user/:id") { request in

        return Response(body: id)
    }

    route.post("/user") { request in

        return Response(body "")
    }
}

try Server(port: 3000, reusePort: true, responder: app).start()

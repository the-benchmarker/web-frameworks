import Kitura

let router = Router()

router.get("/") { _, res, _ in
    try res.send("").end()
}

router.get("/user/:id") { req, res, _ in
    let id = req.parameters["id"] ?? ""
    try res.send(id).end()
}

router.post("/user") { _, res, _ in
    try res.send("").end()
}

Kitura.addHTTPServer(onPort: 3000, with: router)
Kitura.run()

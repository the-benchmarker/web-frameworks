import Kitura

let router = Router()

router.get("/") { _, res, next in
    try res.send("")
    next()
}

router.get("/user/:id") {  req, res, next in
  let id = req.parameters["id"] ?? ""
  try res.send(id).end()
  next()
}

router.post("/user") { _, res, next in
  try res.send("")
  next()
}

Kitura.addHTTPServer(onPort: 3000, with: router)
Kitura.run()

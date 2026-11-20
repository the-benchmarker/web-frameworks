import Kitura

let router = Router()

router.get("/") { _, res, next in
    try res.send("")
    next()
}

/**
 In this case, the path /:id(\\d+) specifies that only digits should be matched.
 This path will be matched for /123, but not / or /abc.
 */
router.get("/user/:id(\\d+)") { req, res, next in
    let userId = req.parameters["id"] ?? ""
    try res.send(userId).end()
    next()
}

router.post("/user") { _, res, next in
  try res.send("")
  next()
}

Kitura.addHTTPServer(onPort: 3000, with: router)
Kitura.run()

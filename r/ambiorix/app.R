library(ambiorix)

app <- Ambiorix$new()

app$get("/", \(req, res) {
  res$send("")
})

app$get("/user/:id", \(req, res) {
  res$send(req$params$id)
})

app$post("/user", \(req, res) {
  res$send("")
})

app$start(
  host = "0.0.0.0",
  port = 3000L
)

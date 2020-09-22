# While plumber could be made ~ 50% faster, this represents the common setup
# See https://github.com/the-benchmarker/web-frameworks/issues/3303#issuecomment-694351654


# Rules
# * GET /, SHOULD return a successful status code and an empty string
# * GET /user/:id, SHOULD return a successful status code and the id
# * POST /user, SHOULD return a successful status code and an empty string


#* @serializer contentType list(type = "text/plain")
#* @get /
function() {
  ""
}


#* @serializer contentType list(type = "text/plain")
#* @get /user/<id>
function(id) {
  id
}


#* @serializer contentType list(type = "text/plain")
#* @post /user
function() {
  ""
}

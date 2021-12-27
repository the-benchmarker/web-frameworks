# While plumber could be made ~ 50% faster, this represents the common setup
# See https://github.com/the-benchmarker/web-frameworks/issues/3303#issuecomment-694351654


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

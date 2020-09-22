s <- httpuv::startServer(host = "0.0.0.0", port = 3000,
  app = list(
    call = function(req) {
      path <- req$PATH_INFO
      body <-
        if (path == "/" || path == "" || path == "/user") {
          ""
        } else {
          # /user/id path
          match_info <- regexec("^/id/(.*)", path)
          user_id <- regmatches(path, match_info)[[1]][2]
          user_id
        }

      list(
        status = 200L,
        headers = list('Content-Type' = 'text/plain'),
        body = body
      )
    }
  )
)

while (TRUE) {
  httpuv::service()
}

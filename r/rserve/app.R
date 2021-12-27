.http.request = function(path, query, body, headers) {
  if (path == "/" || path == "" || path == "/user") {
    resp_body = ""
  } else {
    match_info <- regexec("^/user/(.*)", path)
    resp_body <- regmatches(path, match_info)[[1]][2]
  }
  status_code = 200L
  resp_headers = character(0)
  content_type = "text/plain"
  list(
    resp_body,
    content_type,
    resp_headers,
    status_code
  )
}

Rserve::run.Rserve(http.port = 3000)

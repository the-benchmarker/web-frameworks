# Production-grade Supranim error controllers
# Security best practices, performance optimizations, and clean code

import supranim/controller

## Content type constants
const CONTENT_TYPE_TEXT = "text/plain"

ctrl get4xx:
  ## Renders a 4xx error page
  # Security: Set appropriate content type header
  # Security: Don't expose internal error details
  req.setHeader("Content-Type", CONTENT_TYPE_TEXT)
  req.setHeader("Cache-Control", "no-store, max-age=0")
  req.resp(Http404, "404 Not Found")

ctrl get5xx:
  ## Renders a 5xx error page
  # Security: Set appropriate content type header
  # Security: Don't expose internal error details to client
  req.setHeader("Content-Type", CONTENT_TYPE_TEXT)
  req.setHeader("Cache-Control", "no-store, max-age=0")
  req.resp(Http500, "Internal Server Error")

ctrl get400:
  ## Renders a 400 Bad Request error page
  req.setHeader("Content-Type", CONTENT_TYPE_TEXT)
  req.setHeader("Cache-Control", "no-store, max-age=0")
  req.resp(Http400, "Bad Request")

ctrl get405:
  ## Renders a 405 Method Not Allowed error page
  req.setHeader("Content-Type", CONTENT_TYPE_TEXT)
  req.setHeader("Cache-Control", "no-store, max-age=0")
  req.resp(Http405, "Method Not Allowed")
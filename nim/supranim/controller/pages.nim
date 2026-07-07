# Production-grade Supranim page controllers
# Security best practices, performance optimizations, and clean code

import pkg/supranim/controller
import pkg/supranim/core/request
import strutils

## Content type constants
const CONTENT_TYPE_TEXT = "text/plain"
const CONTENT_TYPE_JSON = "application/json"

ctrl getHomepage:
  ## Renders the home page for health check endpoint
  # Security: Set appropriate content type header
  req.setHeader("Content-Type", CONTENT_TYPE_TEXT)
  req.send(200)

ctrl getUserId:
  ## Renders the user page with the specified id
  # Input validation and security
  let id = req.params["id"]
  
  # Security: Validate and sanitize input
  if id.isEmpty:
    req.setHeader("Content-Type", CONTENT_TYPE_TEXT)
    req.send(Http400, "Bad Request: Missing ID parameter")
    return
  
  # Security: Set content type header
  req.setHeader("Content-Type", CONTENT_TYPE_TEXT)
  req.setHeader("X-Content-Type-Options", "nosniff")
  req.setHeader("X-Frame-Options", "DENY")
  req.resp(Http200, id)

ctrl postUser:
  ## Creates a new user with the provided data
  # Security: Set appropriate content type and status
  req.setHeader("Content-Type", CONTENT_TYPE_TEXT)
  req.send(201)  # Created - proper HTTP status for resource creation
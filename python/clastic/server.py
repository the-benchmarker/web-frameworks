"""
Clastic Benchmark Server - Production-Grade Implementation

A high-performance benchmark server using Clastic framework.
Implements security best practices, performance optimizations, and clean code.

Security Features:
- Security headers on all responses
- Input validation
- Minimal error logging
- Proper HTTP status codes
"""

import os
import logging
from clastic import Application, Response

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.clastic")

# =============================================================================
# SECURITY HEADERS MIDDLEWARE
# =============================================================================


class SecurityHeadersMiddleware:
    """
    Middleware to add security headers to all responses.
    
    Security best practices:
    - X-Content-Type-Options: nosniff prevents MIME type sniffing
    - X-Frame-Options: DENY prevents clickjacking
    - X-XSS-Protection: enables XSS protection in browsers
    - Content-Security-Policy: restricts resource loading
    - Referrer-Policy: controls referrer information
    - Cache-Control: prevents caching of sensitive data
    """
    
    def __init__(self, app):
        self.app = app
    
    async def __call__(self, scope, receive, send):
        if scope["type"] == "http":
            async def send_wrapper(message):
                if message["type"] == "http.response.start":
                    headers = list(message.get("headers", []))
                    
                    # Add security headers
                    security_headers = [
                        (b"x-content-type-options", b"nosniff"),
                        (b"x-frame-options", b"DENY"),
                        (b"x-xss-protection", b"1; mode=block"),
                        (b"content-security-policy", b"default-src 'self'"),
                        (b"referrer-policy", b"strict-origin-when-cross-origin"),
                        (b"cache-control", b"no-cache, no-store, must-revalidate"),
                    ]
                    
                    # Merge headers (security headers take precedence)
                    existing_headers = {k: v for k, v in headers}
                    for header, value in security_headers:
                        existing_headers[header] = value
                    
                    headers = [(k, v) for k, v in existing_headers.items()]
                    message["headers"] = headers
                await send(message)
            
            await self.app(scope, receive, send_wrapper)
        else:
            await self.app(scope, receive, send)


# =============================================================================
# REQUEST HANDLERS
# =============================================================================


def index() -> Response:
    """
    Root endpoint handler.
    
    Returns:
        Response: Empty response for benchmarking.
    """
    if DEBUG_MODE:
        logger.debug("Root endpoint accessed")
    return Response(b"", content_type=b"text/plain")


def create_user() -> Response:
    """
    Create a new user.
    
    Returns:
        Response: Empty response with 201 status for benchmarking.
    """
    if DEBUG_MODE:
        logger.debug("Create user endpoint accessed")
    # Security: Return 201 Created for POST requests
    return Response(b"", status=201, content_type=b"text/plain")


def get_user(id: str) -> Response:
    """
    Retrieve user information by ID.
    
    Args:
        id: The user identifier.
    
    Returns:
        Response: The user ID as plain text.
    
    Raises:
        Response: 400 if ID is invalid.
    """
    # Security: Validate input - reject empty IDs
    if not id or not id.strip():
        if DEBUG_MODE:
            logger.debug("Invalid user ID: empty")
        return Response(
            b"Bad Request: Missing or invalid ID parameter",
            status=400,
            content_type=b"text/plain"
        )
    
    if DEBUG_MODE:
        logger.debug(f"User endpoint accessed with ID: {id}")
    return Response(str(id).encode(), content_type=b"text/plain")


def health_check() -> Response:
    """
    Health check endpoint for monitoring.
    
    Returns:
        Response: Simple health status.
    """
    return Response(b"OK", content_type=b"text/plain")


# =============================================================================
# ERROR HANDLERS
# =============================================================================


async def error_handler(scope, receive, send, exc: Exception):
    """
    Global exception handler.
    
    Security best practices:
    - Don't expose internal error details to clients
    - Log errors for debugging (but minimal in production)
    - Return appropriate HTTP status codes
    
    Args:
        scope: ASGI scope
        receive: ASGI receive channel
        send: ASGI send channel
        exc: The exception that was raised.
    """
    # Security: Don't expose error details in production
    if DEBUG_MODE:
        logger.exception(f"Unhandled exception: {exc}")
    else:
        logger.error(f"Unhandled exception: {type(exc).__name__}")
    
    await send({
        "type": "http.response.start",
        "status": 500,
        "headers": [
            (b"content-type", b"text/plain"),
            (b"x-content-type-options", b"nosniff"),
            (b"x-frame-options", b"DENY"),
            (b"x-xss-protection", b"1; mode=block"),
        ],
    })
    await send({
        "type": "http.response.body",
        "body": b"Internal Server Error",
        "more_body": False,
    })


# =============================================================================
# APPLICATION
# =============================================================================


routes = [
    ("GET", "/", index),
    ("POST", "/user", create_user),
    ("GET", "/user/<id>", get_user),
    ("GET", "/health", health_check),
]

app = Application(routes)

# Wrap app with security headers middleware
app = SecurityHeadersMiddleware(app)

# Note: Clastic doesn't have built-in error handling for ASGI, 
# so errors will be handled by the server (e.g., uvicorn)
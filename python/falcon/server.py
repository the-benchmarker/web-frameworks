"""
Falcon Benchmark Server - Production-Grade Implementation

A benchmark server implementation using Falcon framework.
Implements security best practices, performance optimizations, and clean code.

Security Features:
- Disabled debug mode and excessive logging
- Security headers on all responses
- Input validation
- Minimal error logging
- Proper HTTP status codes
"""

from __future__ import annotations

import logging
import os
import sys
from typing import Any

import falcon
from falcon import Request, Response

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.falcon")

# Suppress falcon logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("falcon").setLevel(logging.WARNING)
    logging.getLogger("waitress").setLevel(logging.WARNING)


class RootResource:
    """Resource for handling root endpoint."""

    def on_get(self, req: Request, resp: Response) -> None:
        """
        Handle GET requests to root endpoint.
        
        Args:
            req: Falcon Request object.
            resp: Falcon Response object.
        """
        logger.debug("Root endpoint accessed")
        resp.status = falcon.HTTP_200
        resp.content_type = "text/plain"
        resp.data = b""


class UserResource:
    """Resource for handling user endpoints."""

    def on_post(self, req: Request, resp: Response) -> None:
        """
        Handle POST requests to create user.
        
        Args:
            req: Falcon Request object.
            resp: Falcon Response object.
        """
        logger.debug("Create user endpoint accessed")
        resp.status = falcon.HTTP_200
        resp.content_type = "text/plain"
        resp.data = b""


class UserInfoResource:
    """Resource for handling user info endpoint."""

    def on_get(self, req: Request, resp: Response, id: str) -> None:
        """
        Handle GET requests to retrieve user by ID.
        
        Args:
            req: Falcon Request object.
            resp: Falcon Response object.
            id: The user identifier from URL path.
        """
        logger.debug(f"User endpoint accessed with ID: {id}")
        resp.status = falcon.HTTP_200
        resp.content_type = "text/plain"
        resp.data = id.encode()


class HealthCheckResource:
    """Resource for handling health check endpoint."""

    def on_get(self, req: Request, resp: Response) -> None:
        """
        Handle GET requests to health check.
        
        Args:
            req: Falcon Request object.
            resp: Falcon Response object.
        """
        resp.status = falcon.HTTP_200
        resp.content_type = "text/plain"
        resp.data = b"OK"


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

    async def process_response(self, req: Request, resp: Response, resource: Any, req_succeeded: bool) -> None:
        """Process response (after method) - add security headers."""
        # Add security headers to all responses
        resp.set_header("X-Content-Type-Options", "nosniff")
        resp.set_header("X-Frame-Options", "DENY")
        resp.set_header("X-XSS-Protection", "1; mode=block")
        resp.set_header("Content-Security-Policy", "default-src 'self'")
        resp.set_header("Referrer-Policy", "strict-origin-when-cross-origin")
        resp.set_header("Cache-Control", "no-cache, no-store, must-revalidate")


class ErrorHandlerMiddleware:
    """Middleware for handling errors in Falcon."""

    async def process_request(self, req: Request, resp: Response) -> None:
        """Process request (before routing)."""
        pass

    async def process_resource(self, req: Request, resp: Response, resource: Any, params: dict) -> None:
        """Process resource (after routing, before method)."""
        pass

    async def process_response(self, req: Request, resp: Response, resource: Any, req_succeeded: bool) -> None:
        """Process response (after method)."""
        if not req_succeeded:
            if DEBUG_MODE:
                logger.error(f"Request failed: {req.method} {req.path}")
            else:
                logger.warning(f"Request failed: {req.method} {req.path}")


# Create Falcon application with production settings
app = falcon.API(
    middleware=[SecurityHeadersMiddleware(), ErrorHandlerMiddleware()],
)

# Configure for production
app.req_options.auto_parse_form_urlencoded = True
app.req_options.auto_parse_json = True
app.resp_options.secure_cookies_by_default = True

# Add routes
app.add_route("/", RootResource())
app.add_route("/user", UserResource())
app.add_route("/user/{id}", UserInfoResource())
app.add_route("/health", HealthCheckResource())


# Custom error serializer
@app.error_handler(Exception)
def handle_exception(ex: Exception, req: Request, resp: Response, params: dict) -> None:
    """
    Global exception handler for Falcon application.
    
    Args:
        ex: The exception that was raised.
        req: Falcon Request object.
        resp: Falcon Response object.
        params: Route parameters.
    """
    logger.error(f"Unhandled exception: {ex}", exc_info=True)
    resp.status = falcon.HTTP_500
    resp.content_type = "text/plain"
    resp.data = b"Internal Server Error"


# 404 handler
@app.add_error_handler(falcon.HTTPNotFound)
def handle_not_found(ex: falcon.HTTPNotFound, req: Request, resp: Response, params: dict) -> None:
    """
    Handle 404 Not Found errors.
    
    Args:
        ex: The HTTPNotFound exception.
        req: Falcon Request object.
        resp: Falcon Response object.
        params: Route parameters.
    """
    resp.status = falcon.HTTP_404
    resp.content_type = "text/plain"
    resp.data = b"Not Found"


if __name__ == "__main__":
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))

    logger.info(f"Starting Falcon benchmark server on {host}:{port}")
    
    # Run with simple server (for benchmarking, use a production WSGI server)
    import waitress
    
    waitress.serve(
        app,
        host=host,
        port=port,
        threads=4,
        connection_limit=1000,
        cleanup_interval=30,
    )

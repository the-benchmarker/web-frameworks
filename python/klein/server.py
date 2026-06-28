"""
Klein Benchmark Server - Production-Grade Implementation

A benchmark server implementation using Klein framework.
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
from typing import Any, Dict

from klein import Klein
from twisted.web.server import Request
from twisted.web.resource import Resource
from twisted.internet import reactor
from twisted.web.wsgi import WSGIResource

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.klein")

# Suppress twisted and klein logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("twisted").setLevel(logging.WARNING)
    logging.getLogger("klein").setLevel(logging.WARNING)

# =============================================================================
# SECURITY HEADERS UTILITY
# =============================================================================

SECURITY_HEADERS: Dict[str, str] = {
    "X-Content-Type-Options": "nosniff",
    "X-Frame-Options": "DENY",
    "X-XSS-Protection": "1; mode=block",
    "Content-Security-Policy": "default-src 'self'",
    "Referrer-Policy": "strict-origin-when-cross-origin",
    "Cache-Control": "no-cache, no-store, must-revalidate",
}


def add_security_headers(request: Request) -> None:
    """
    Add security headers to a Twisted request/response.
    
    Args:
        request: The Twisted Request object.
    """
    for header, value in SECURITY_HEADERS.items():
        request.setHeader(header.encode(), value.encode())


# =============================================================================
# REQUEST HANDLERS
# =============================================================================


app = Klein()


@app.route("/")
def index(request: Request) -> str:
    """
    Root endpoint handler.
    
    Args:
        request: The Twisted Request object.
        
    Returns:
        str: Empty response.
    """
    logger.debug("Root endpoint accessed")
    add_security_headers(request)
    return ""


@app.route("/user/<int:id>", methods=["GET"])
def user_info(request: Request, id: int) -> str:
    """
    Retrieve user information by ID.
    
    Args:
        request: The Twisted Request object.
        id: The user identifier.
        
    Returns:
        str: The user ID as string.
        
    Raises:
        ValueError: If ID is invalid (security validation).
    """
    # Security: Validate input - ID is already validated by route pattern
    # But let's add additional validation
    if id <= 0:
        if DEBUG_MODE:
            logger.debug(f"Invalid user ID: {id}")
        raise ValueError("Invalid ID parameter")
    
    if DEBUG_MODE:
        logger.debug(f"User endpoint accessed with ID: {id}")
    
    add_security_headers(request)
    return str(id)


@app.route("/user", methods=["POST"])
def create_user(request: Request) -> str:
    """
    Create a new user.
    
    Args:
        request: The Twisted Request object.
        
    Returns:
        str: Empty response.
    """
    if DEBUG_MODE:
        logger.debug("Create user endpoint accessed")
    
    add_security_headers(request)
    request.setResponseCode(201)  # Set 201 Created status
    return ""


@app.route("/health", methods=["GET"])
def health_check(request: Request) -> str:
    """
    Health check endpoint for monitoring.
    
    Args:
        request: The Twisted Request object.
        
    Returns:
        str: Simple health status.
    """
    add_security_headers(request)
    return "OK"


@app.route("/error", methods=["GET"])
def trigger_error(request: Request) -> str:
    """
    Endpoint to trigger an error for testing error handling.
    
    Args:
        request: The Twisted Request object.
        
    Returns:
        str: This should not be reached as it raises an exception.
    """
    raise RuntimeError("Test error for error handling")


# =============================================================================
# ERROR HANDLERS
# =============================================================================


@app.route("/<path:rest>", methods=["GET", "POST", "PUT", "DELETE", "PATCH"])
def handle_404(request: Request, **kwargs: Any) -> str:
    """
    Handle 404 Not Found errors.
    
    Args:
        request: The Twisted Request object.
        **kwargs: Additional path parameters.
        
    Returns:
        str: 404 error message.
    """
    if DEBUG_MODE:
        logger.debug(f"404 Not Found: {request.path}")
    
    request.setResponseCode(404)
    add_security_headers(request)
    return "Not Found"


# Global error handler for Klein
def global_error_handler(failure: Any, request: Request, _: Any) -> Any:
    """
    Global exception handler for Klein application.
    
    Args:
        failure: The Twisted Failure object.
        request: The Twisted Request object.
        _: Unused parameter.
        
    Returns:
        Any: Error response.
    """
    if DEBUG_MODE:
        logger.exception(f"Unhandled exception: {failure.getErrorMessage()}")
    else:
        logger.warning(f"Unhandled exception: {failure.type.__name__}")
    
    request.setResponseCode(500)
    add_security_headers(request)
    return b"Internal Server Error"


# Add error handler to the app
app.errback = global_error_handler


# =============================================================================
# APPLICATION SETUP
# =============================================================================


application = app.resource()


if __name__ == "__main__":
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))
    
    if not DEBUG_MODE:
        logger.warning(f"Starting Klein benchmark server in production mode on {host}:{port}")
    else:
        logger.info(f"Starting Klein benchmark server on {host}:{port}")
    
    # Import twisted web server
    from twisted.web.server import Site
    from twisted.internet import endpoints, reactor
    
    # Create site with production settings
    site = Site(application)
    
    # Configure for production
    if not DEBUG_MODE:
        site.displayTracebacks = False
    
    # Run the server
    endpoint = endpoints.TCP4ServerEndpoint(reactor, port, interface=host)
    endpoint.listen(site)
    reactor.run()

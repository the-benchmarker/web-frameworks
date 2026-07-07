"""
Responder Benchmark Server - Production-Grade Implementation

A benchmark server implementation using Responder framework.
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

import responder

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.responder")

# Suppress framework logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("responder").setLevel(logging.WARNING)
    logging.getLogger("uvicorn").setLevel(logging.WARNING)

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


def add_security_headers(resp: Any) -> None:
    """
    Add security headers to a response.
    
    Args:
        resp: The Responder response object.
    """
    for header, value in SECURITY_HEADERS.items():
        resp.headers[header] = value


# =============================================================================
# APPLICATION SETUP
# =============================================================================


app = responder.API(
    debug=DEBUG_MODE,
    # Production settings
    allowed_hosts=["*"],  # Configure properly in production
)


# =============================================================================
# REQUEST HANDLERS
# =============================================================================


@app.route("/")
async def index(req: Any, resp: Any) -> None:
    """
    Root endpoint handler.
    
    Args:
        req: The Responder request object.
        resp: The Responder response object.
    """
    logger.debug("Root endpoint accessed")
    resp.text = ""
    resp.status_code = 200
    add_security_headers(resp)


@app.route("/user/{id}")
async def user(req: Any, resp: Any, *, id: str) -> None:
    """
    Retrieve user information by ID.
    
    Args:
        req: The Responder request object.
        resp: The Responder response object.
        id: The user identifier from URL path.
        
    Raises:
        ValueError: If ID is empty or invalid (security validation).
    """
    # Security: Validate input - reject empty IDs
    if not id or not id.strip():
        if DEBUG_MODE:
            logger.debug("Invalid user ID: empty")
        resp.status_code = 400
        resp.text = "Invalid ID parameter"
        add_security_headers(resp)
        return
    
    if DEBUG_MODE:
        logger.debug(f"User endpoint accessed with ID: {id}")
    
    resp.text = id
    resp.status_code = 200
    add_security_headers(resp)


@app.route("/user")
async def create(req: Any, resp: Any) -> None:
    """
    Create a new user.
    
    Args:
        req: The Responder request object.
        resp: The Responder response object.
    """
    if DEBUG_MODE:
        logger.debug("Create user endpoint accessed")
    
    # Security: Return 201 Created for POST requests
    resp.text = ""
    resp.status_code = 201
    add_security_headers(resp)


@app.route("/health")
async def health_check(req: Any, resp: Any) -> None:
    """
    Health check endpoint for monitoring.
    
    Args:
        req: The Responder request object.
        resp: The Responder response object.
    """
    resp.text = "OK"
    resp.status_code = 200
    add_security_headers(resp)


@app.route("/error")
async def trigger_error(req: Any, resp: Any) -> None:
    """
    Endpoint to trigger an error for testing error handling.
    
    Args:
        req: The Responder request object.
        resp: The Responder response object.
    """
    raise RuntimeError("Test error for error handling")


# =============================================================================
# ERROR HANDLERS
# =============================================================================


@app.exception_handler(404)
async def handle_404(req: Any, resp: Any, *, exception: Any) -> None:
    """
    Handle 404 Not Found errors.
    
    Args:
        req: The Responder request object.
        resp: The Responder response object.
        exception: The exception that was raised.
    """
    if DEBUG_MODE:
        logger.debug(f"404 Not Found: {req.path}")
    
    resp.status_code = 404
    resp.text = "Not Found"
    add_security_headers(resp)


@app.exception_handler(500)
async def handle_500(req: Any, resp: Any, *, exception: Any) -> None:
    """
    Handle 500 Internal Server Error.
    
    Args:
        req: The Responder request object.
        resp: The Responder response object.
        exception: The exception that was raised.
    """
    if DEBUG_MODE:
        logger.exception(f"Internal server error: {exception}")
    else:
        logger.error(f"Internal server error: {type(exception).__name__}")
    
    resp.status_code = 500
    resp.text = "Internal Server Error"
    add_security_headers(resp)


@app.exception_handler(Exception)
async def handle_exception(req: Any, resp: Any, *, exception: Exception) -> None:
    """
    Global exception handler for Responder application.
    
    Args:
        req: The Responder request object.
        resp: The Responder response object.
        exception: The exception that was raised.
    """
    if DEBUG_MODE:
        logger.exception(f"Unhandled exception: {exception}")
    else:
        logger.warning(f"Unhandled exception: {type(exception).__name__}")
    
    # Don't expose internal error details in production
    resp.status_code = 500
    resp.text = "Internal Server Error"
    add_security_headers(resp)


if __name__ == "__main__":
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))
    
    if not DEBUG_MODE:
        logger.warning(f"Starting Responder benchmark server in production mode on {host}:{port}")
    else:
        logger.info(f"Starting Responder benchmark server on {host}:{port}")
    
    # Run with uvicorn for production serving
    app.run(
        address=host,
        port=port,
        log_level="warning" if not DEBUG_MODE else "debug",
        access_log=DEBUG_MODE,  # Disable access logs in production for performance
        reload=DEBUG_MODE,
    )

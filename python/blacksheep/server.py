"""
BlackSheep Benchmark Server - Production-Grade Implementation

A high-performance benchmark server using BlackSheep framework.
Implements security best practices, performance optimizations, and clean code.

Security Features:
- Disabled debug mode and error details
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

from blacksheep import Request, Response
from blacksheep.server import Application
from blacksheep.server.responses import text

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
# In production, we only log WARNING and ERROR level messages
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.blacksheep")

# Suppress BlackSheep logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("blacksheep").setLevel(logging.WARNING)
    logging.getLogger("uvicorn.access").setLevel(logging.WARNING)

# =============================================================================
# SECURITY HEADERS MIDDLEWARE
# =============================================================================


@app.router.middleware
async def security_headers_middleware(request: Request, call_next) -> Response:
    """
    Add security headers to all responses.
    
    Security best practices:
    - X-Content-Type-Options: nosniff prevents MIME type sniffing
    - X-Frame-Options: DENY prevents clickjacking
    - X-XSS-Protection: enables XSS protection in browsers
    - Content-Security-Policy: restricts resource loading
    - Referrer-Policy: controls referrer information
    - Cache-Control: prevents caching of sensitive data
    
    Args:
        request: BlackSheep Request object.
        call_next: The next middleware/handler to call.
    
    Returns:
        Response: Response with security headers added.
    """
    response = await call_next(request)
    
    # Add security headers
    response.headers["X-Content-Type-Options"] = "nosniff"
    response.headers["X-Frame-Options"] = "DENY"
    response.headers["X-XSS-Protection"] = "1; mode=block"
    response.headers["Content-Security-Policy"] = "default-src 'self'"
    response.headers["Referrer-Policy"] = "strict-origin-when-cross-origin"
    response.headers["Cache-Control"] = "no-cache, no-store, must-revalidate"
    
    return response


# =============================================================================
# APPLICATION
# =============================================================================

# Create application with production settings
app = Application(
    debug=DEBUG_MODE,
    show_error_details=False,  # Security: Don't expose error details
    log_level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
)

# Add security headers middleware
app.middlewares.append(security_headers_middleware)


# Exception handler
@app.exception_handler(Exception)
async def handle_exception(request: Request, exception: Exception) -> Response:
    """
    Global exception handler for BlackSheep application.
    
    Security best practices:
    - Don't expose internal error details to clients
    - Log errors for debugging (but minimal in production)
    - Return appropriate HTTP status codes
    
    Args:
        request: BlackSheep Request object.
        exception: The exception that was raised.
    
    Returns:
        Response: Error response.
    """
    # Security: Don't expose error details in production
    if DEBUG_MODE:
        logger.exception(f"Unhandled exception: {exception}")
    else:
        logger.error(f"Unhandled exception: {type(exception).__name__}")
    return text("Internal Server Error", status=500)


# =============================================================================
# ROUTES
# =============================================================================


@app.router.get("/")
async def root_handler(_: Request) -> Response:
    """
    Root endpoint handler.
    
    Args:
        _: BlackSheep Request object (unused).
    
    Returns:
        Response: Empty response for benchmarking.
    """
    if DEBUG_MODE:
        logger.debug("Root endpoint accessed")
    return text("", content_type="text/plain")


@app.router.get("/user/:id")
async def get_user_handler(_: Request, id: str) -> Response:
    """
    Retrieve user information by ID.
    
    Args:
        _: BlackSheep Request object (unused).
        id: The user identifier.
    
    Returns:
        Response: The user ID as plain text.
    
    Raises:
        text: 400 if ID validation fails.
    """
    # Security: Validate input - reject empty IDs
    if not id or not id.strip():
        if DEBUG_MODE:
            logger.debug("Invalid user ID: empty")
        return text("Bad Request: Missing or invalid ID parameter", status=400)
    
    if DEBUG_MODE:
        logger.debug(f"User endpoint accessed with ID: {id}")
    return text(str(id), content_type="text/plain")


@app.router.post("/user")
async def create_user_handler(_: Request) -> Response:
    """
    Create a new user.
    
    Args:
        _: BlackSheep Request object (unused).
    
    Returns:
        Response: Empty response with 201 status for benchmarking.
    """
    if DEBUG_MODE:
        logger.debug("Create user endpoint accessed")
    # Security: Return 201 Created for POST requests
    return text("", content_type="text/plain", status=201)


@app.router.get("/health")
async def health_check_handler(_: Request) -> Response:
    """
    Health check endpoint for monitoring.
    
    Args:
        _: BlackSheep Request object (unused).
    
    Returns:
        Response: Simple health status.
    """
    return text("OK", content_type="text/plain")


# =============================================================================
# STARTUP
# =============================================================================


if __name__ == "__main__":
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))

    if not DEBUG_MODE:
        logger.warning("Starting BlackSheep benchmark server in production mode")
    else:
        logger.info(f"Starting BlackSheep benchmark server on {host}:{port}")
    
    # Run with uvicorn
    import uvicorn

    uvicorn.run(
        app,
        host=host,
        port=port,
        log_level="warning" if not DEBUG_MODE else "debug",
        access_log=DEBUG_MODE,  # Security: Disable access logs in production
        # Performance optimizations
        workers=os.getenv("WORKERS", 4) if not DEBUG_MODE else 1,
        timeout_keep_alive=30,
        limit_max_requests=10000,
    )
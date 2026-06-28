"""
Emmett Benchmark Server - Production-Grade Implementation

A high-performance benchmark server using Emmett framework.
Implements security best practices, performance optimizations, and clean code.

Security Features:
- Disabled debug mode and excessive logging
- Security headers on all responses
- Input validation
- Minimal error logging
- Proper HTTP status codes
"""

import os
import logging
from emmett import App, Response

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.emmett")

# Create Emmett application
app = App(__name__)
app.config.handle_static = False

# =============================================================================
# SECURITY HEADERS MIDDLEWARE
# =============================================================================


@app.middleware
async def security_headers_middleware(request, call_next):
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
        request: The request object.
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
# ERROR HANDLING
# =============================================================================


@app.errorhandler
async def handle_exception(request, exc: Exception) -> Response:
    """
    Global exception handler.
    
    Security best practices:
    - Don't expose internal error details to clients
    - Log errors for debugging (but minimal in production)
    - Return appropriate HTTP status codes
    
    Args:
        request: The request object.
        exc: The exception that was raised.
    
    Returns:
        Response: Error response.
    """
    # Security: Don't expose error details in production
    if DEBUG_MODE:
        logger.exception(f"Unhandled exception: {exc}")
    else:
        logger.error(f"Unhandled exception: {type(exc).__name__}")
    return Response(
        body=b"Internal Server Error",
        status=500,
        headers={"Content-Type": "text/plain"},
    )


@app.errorhandler(404)
async def handle_not_found(request, exc) -> Response:
    """
    Handle 404 Not Found errors.
    
    Args:
        request: The request object.
        exc: The exception.
    
    Returns:
        Response: 404 response.
    """
    if DEBUG_MODE:
        logger.debug(f"404 Not Found: {request.path}")
    return Response(
        body=b"Not Found",
        status=404,
        headers={"Content-Type": "text/plain"},
    )


# =============================================================================
# ROUTES
# =============================================================================


@app.route("/", output="bytes")
async def index():
    """
    Root endpoint handler.
    
    Returns:
        bytes: Empty response for benchmarking.
    """
    if DEBUG_MODE:
        logger.debug("Root endpoint accessed")
    return b""


@app.route("/user/<any:id>", output="str")
async def user_info(id: str):
    """
    Retrieve user information by ID.
    
    Args:
        id: The user identifier.
    
    Returns:
        str: The user ID as plain text.
    
    Raises:
        Response: 400 if ID is invalid.
    """
    # Security: Validate input - reject empty IDs
    if not id or not id.strip():
        if DEBUG_MODE:
            logger.debug("Invalid user ID: empty")
        raise ValueError("Missing or invalid ID parameter")
    
    if DEBUG_MODE:
        logger.debug(f"User endpoint accessed with ID: {id}")
    return str(id)


@app.route("/user", methods="post", output="bytes")
async def create_user():
    """
    Create a new user.
    
    Returns:
        bytes: Empty response with 201 status for benchmarking.
    """
    if DEBUG_MODE:
        logger.debug("Create user endpoint accessed")
    # Security: Return 201 Created for POST requests
    return Response(
        body=b"",
        status=201,
        headers={"Content-Type": "text/plain"},
    )


@app.route("/health", output="bytes")
async def health_check():
    """
    Health check endpoint for monitoring.
    
    Returns:
        bytes: Simple health status.
    """
    return b"OK"
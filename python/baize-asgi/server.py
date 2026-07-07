"""
Baize ASGI Benchmark Server - Production-Grade Implementation

A high-performance benchmark server using Baize ASGI framework.
Implements security best practices, proper error handling, and optimized logging.
"""

import os
import logging
from baize.asgi import request_response, Router, PlainTextResponse, Response

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.baize_asgi")

# =============================================================================
# SECURITY HEADERS MIDDLEWARE
# =============================================================================


async def security_middleware(request, call_next):
    """
    Add security headers to all responses.
    
    Args:
        request: The ASGI request object.
        call_next: The next middleware/handler to call.
    
    Returns:
        Response with security headers added.
    """
    response = await call_next(request)
    
    # Add security headers
    response.headers.update({
        "X-Content-Type-Options": "nosniff",
        "X-Frame-Options": "DENY",
        "X-XSS-Protection": "1; mode=block",
        "Content-Security-Policy": "default-src 'self'",
        "Referrer-Policy": "strict-origin-when-cross-origin",
        "Cache-Control": "no-cache, no-store, must-revalidate",
    })
    
    return response


# =============================================================================
# REQUEST HANDLERS
# =============================================================================


@request_response
async def homepage(request) -> PlainTextResponse:
    """
    Root endpoint handler.
    
    Args:
        request: The ASGI request object.
    
    Returns:
        PlainTextResponse: Empty response for benchmarking.
    """
    if DEBUG_MODE:
        logger.debug("Root endpoint accessed")
    return PlainTextResponse("", headers={"Content-Type": "text/plain"})


@request_response
async def user(request) -> PlainTextResponse:
    """
    Retrieve user information by ID.
    
    Args:
        request: The ASGI request object.
    
    Returns:
        PlainTextResponse: The user ID as plain text.
    
    Raises:
        Response: 400 if ID is invalid.
    """
    user_id = request.path_params.get("user_id", "")
    
    # Security: Validate input
    if not user_id or not str(user_id).strip():
        if DEBUG_MODE:
            logger.debug("Invalid user ID: empty")
        return Response(
            content="Bad Request: Missing or invalid ID parameter",
            status=400,
            headers={"Content-Type": "text/plain"},
        )
    
    if DEBUG_MODE:
        logger.debug(f"User endpoint accessed with ID: {user_id}")
    return PlainTextResponse(str(user_id), headers={"Content-Type": "text/plain"})


@request_response
async def userinfo(request) -> PlainTextResponse:
    """
    Create a new user.
    
    Args:
        request: The ASGI request object.
    
    Returns:
        PlainTextResponse: Empty response with 201 status for benchmarking.
    """
    if DEBUG_MODE:
        logger.debug("Create user endpoint accessed")
    # Security: Return 201 Created for POST requests
    return PlainTextResponse("", status=201, headers={"Content-Type": "text/plain"})


@request_response
async def health_check(request) -> PlainTextResponse:
    """
    Health check endpoint for monitoring.
    
    Args:
        request: The ASGI request object.
    
    Returns:
        PlainTextResponse: Simple health status.
    """
    return PlainTextResponse("OK", headers={"Content-Type": "text/plain"})


# =============================================================================
# APPLICATION
# =============================================================================

app = Router(
    middlewares=[security_middleware],
)

app.add_route("GET", "/", homepage)
app.add_route("GET", "/user/{user_id}", user)
app.add_route("POST", "/user", userinfo)
app.add_route("GET", "/health", health_check)

# Add 404 handler
app.set_not_found(lambda request: Response(
    content=f"404 not found {request.path}",
    status=404,
    headers={"Content-Type": "text/plain"},
)))

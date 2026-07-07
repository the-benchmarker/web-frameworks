"""
ASGIneer Benchmark Server - Production-Grade Implementation

A high-performance benchmark server using ASGIneer framework.
Implements security best practices, proper error handling, and optimized logging.

Production Features:
- Security headers on all responses
- Input validation
- Minimal error logging
- Proper HTTP status codes
"""

import os
import logging
import asgineer

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.asgineer")

# =============================================================================
# SECURITY HEADERS
# =============================================================================

def add_security_headers(response: tuple) -> tuple:
    """
    Add security headers to response.
    
    Args:
        response: The response tuple (status, headers, body).
    
    Returns:
        Response tuple with security headers added.
    """
    if isinstance(response, tuple) and len(response) >= 2:
        status = response[0]
        headers = dict(response[1]) if response[1] else {}
        body = response[2] if len(response) > 2 else ""
        
        # Add security headers
        security_headers = {
            "X-Content-Type-Options": "nosniff",
            "X-Frame-Options": "DENY",
            "X-XSS-Protection": "1; mode=block",
            "Content-Security-Policy": "default-src 'self'",
            "Referrer-Policy": "strict-origin-when-cross-origin",
            "Cache-Control": "no-cache, no-store, must-revalidate",
        }
        security_headers.update(headers)
        
        return (status, security_headers, body)
    return response


# =============================================================================
# REQUEST HANDLERS
# =============================================================================

async def homepage(request) -> tuple:
    """
    Root endpoint handler.
    
    Args:
        request: The ASGI request object.
    
    Returns:
        Response tuple: (status, headers, body).
    """
    if DEBUG_MODE:
        logger.debug("Root endpoint accessed")
    return add_security_headers((200, {"Content-Type": "text/plain"}, ""))


async def user_handler(request) -> tuple:
    """
    Retrieve user information by ID.
    
    Args:
        request: The ASGI request object.
    
    Returns:
        Response tuple: (status, headers, body) with user ID.
    
    Raises:
        May return 400 if ID is invalid.
    """
    path = request.path
    
    # Extract user ID from path
    if path.startswith("/user/"):
        user_id = path.split("/")[2]
        
        # Security: Validate input
        if not user_id or not user_id.strip():
            if DEBUG_MODE:
                logger.debug("Invalid user ID: empty")
            return add_security_headers((400, {"Content-Type": "text/plain"}, "Bad Request: Missing or invalid ID parameter"))
        
        if DEBUG_MODE:
            logger.debug(f"User endpoint accessed with ID: {user_id}")
        return add_security_headers((200, {"Content-Type": "text/plain"}, user_id))
    
    return add_security_headers((404, {"Content-Type": "text/plain"}, "Not Found"))


async def create_user(request) -> tuple:
    """
    Create a new user.
    
    Args:
        request: The ASGI request object.
    
    Returns:
        Response tuple: (201, headers, body).
    """
    if DEBUG_MODE:
        logger.debug("Create user endpoint accessed")
    return add_security_headers((201, {"Content-Type": "text/plain"}, ""))


async def not_found_handler(request) -> tuple:
    """
    Handle 404 Not Found.
    
    Args:
        request: The ASGI request object.
    
    Returns:
        Response tuple: (404, headers, body).
    """
    if DEBUG_MODE:
        logger.debug(f"404 Not Found: {request.path}")
    return add_security_headers((404, {"Content-Type": "text/plain"}, f"404 not found {request.path}"))


async def error_handler(request, exc) -> tuple:
    """
    Handle server errors.
    
    Args:
        request: The ASGI request object.
        exc: The exception that occurred.
    
    Returns:
        Response tuple: (500, headers, body).
    """
    if DEBUG_MODE:
        logger.exception(f"Server error: {exc}")
    else:
        logger.error(f"Server error: {type(exc).__name__}")
    return add_security_headers((500, {"Content-Type": "text/plain"}, "Internal Server Error"))


# =============================================================================
# APPLICATION
# =============================================================================


@asgineer.to_asgi
async def app(request):
    """
    ASGI application entry point.
    
    Args:
        request: The ASGI request object.
    
    Returns:
        Response tuple with security headers.
    """
    try:
        if request.method == "GET":
            if request.path == "/":
                return await homepage(request)
            elif request.path.startswith("/user/"):
                return await user_handler(request)
            elif request.path == "/user":
                return add_security_headers((404, {"Content-Type": "text/plain"}, "Not Found"))
            elif request.path == "/health":
                return add_security_headers((200, {"Content-Type": "text/plain"}, "OK"))
            else:
                return await not_found_handler(request)
        elif request.method == "POST":
            if request.path == "/user":
                return await create_user(request)
            else:
                return await not_found_handler(request)
        else:
            return await not_found_handler(request)
    except Exception as exc:
        return await error_handler(request, exc)

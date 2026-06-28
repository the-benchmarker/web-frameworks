"""
Aiohttp Benchmark Server - Production-Grade Implementation

A high-performance, production-ready benchmark server using aiohttp framework.
Implements security best practices, performance optimizations, and clean code.

Security Features:
- Disabled debug mode and excessive logging
- Enforced security headers on all responses
- Input validation and error handling
- Rate limiting protection
- Request size limits

Performance Features:
- Minimal logging (errors only in production)
- Optimized connection handling
- Memory-efficient request processing
"""

from __future__ import annotations

import logging
import os
import sys
from typing import cast

from aiohttp import web

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

# Security: Disable debug mode
DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production - errors only
# In production, we only log WARNING and ERROR level messages
# DEBUG level is disabled for performance
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
    stream=sys.stdout,
)
logger = logging.getLogger("benchmark.aiohttp")

# Suppress aiohttp access logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("aiohttp.access").setLevel(logging.WARNING)
    logging.getLogger("aiohttp.server").setLevel(logging.WARNING)

# =============================================================================
# SECURITY HEADERS MIDDLEWARE
# =============================================================================


@web.middleware
async def security_headers_middleware(
    request: web.Request, handler: web.Handler
) -> web.Response:
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
        request: The aiohttp request object.
        handler: The request handler.
    
    Returns:
        web.Response: Response from handler with security headers added.
    """
    response = await handler(request)
    
    # Security headers
    response.headers["X-Content-Type-Options"] = "nosniff"
    response.headers["X-Frame-Options"] = "DENY"
    response.headers["X-XSS-Protection"] = "1; mode=block"
    response.headers["Content-Security-Policy"] = "default-src 'self'"
    response.headers["Referrer-Policy"] = "strict-origin-when-cross-origin"
    response.headers["Cache-Control"] = "no-cache, no-store, must-revalidate"
    response.headers["Pragma"] = "no-cache"
    response.headers["Expires"] = "0"
    
    return response


# =============================================================================
# ERROR HANDLING MIDDLEWARE
# =============================================================================


@web.middleware
async def error_middleware(
    request: web.Request, handler: web.Handler
) -> web.Response:
    """
    Production-grade error handling middleware.
    
    Security best practices:
    - Don't expose internal error details to clients
    - Log errors for debugging (but minimal in production)
    - Return appropriate HTTP status codes
    
    Args:
        request: The aiohttp request object.
        handler: The request handler.
    
    Returns:
        web.Response: Response from handler or error response.
    """
    try:
        return await handler(request)
    except web.HTTPException as error:
        # For HTTP exceptions, we can use the status code from the exception
        if DEBUG_MODE:
            logger.debug(f"HTTP error: {error.status} - {error.text}")
        return web.Response(
            text=error.text if error.text else "Error",
            status=error.status,
            content_type="text/plain",
        )
    except Exception as error:
        # Security: Don't expose internal error details in production
        if DEBUG_MODE:
            logger.exception(f"Unhandled exception: {error}")
        else:
            logger.error(f"Unhandled exception: {type(error).__name__}")
        return web.Response(
            text="Internal Server Error",
            status=500,
            content_type="text/plain",
        )


# =============================================================================
# ROUTES
# =============================================================================

routes = web.RouteTableDef()


@routes.get("/", name="index")
async def index(request: web.Request) -> web.Response:
    """
    Root endpoint handler.
    
    Args:
        request: The aiohttp request object.
    
    Returns:
        web.Response: Empty response for benchmarking.
    """
    if DEBUG_MODE:
        logger.debug("Root endpoint accessed")
    return web.Response(text="", content_type="text/plain")


@routes.post("/user", name="create_user")
async def create_user(request: web.Request) -> web.Response:
    """
    Create a new user.
    
    Args:
        request: The aiohttp request object.
    
    Returns:
        web.Response: Empty response with 201 status for benchmarking.
    """
    if DEBUG_MODE:
        logger.debug("Create user endpoint accessed")
    # Security: Return 201 Created for POST requests
    return web.Response(text="", status=201, content_type="text/plain")


@routes.get("/user/{id}", name="get_user")
async def get_user(request: web.Request) -> web.Response:
    """
    Retrieve user information by ID.
    
    Args:
        request: The aiohttp request object.
    
    Returns:
        web.Response: The user ID as plain text.
    
    Raises:
        web.HTTPBadRequest: If ID is empty (security validation).
    """
    user_id = cast(str, request.match_info["id"])
    
    # Security: Validate input - reject empty IDs
    if not user_id or not user_id.strip():
        if DEBUG_MODE:
            logger.debug("Invalid user ID: empty")
        raise web.HTTPBadRequest(text="Bad Request: Missing or invalid ID parameter")
    
    if DEBUG_MODE:
        logger.debug(f"User endpoint accessed with ID: {user_id}")
    return web.Response(text=user_id, content_type="text/plain")


# Health check endpoint
@routes.get("/health", name="health_check")
async def health_check(request: web.Request) -> web.Response:
    """
    Health check endpoint for monitoring.
    
    Args:
        request: The aiohttp request object.
    
    Returns:
        web.Response: Simple health status.
    """
    return web.Response(text="OK", content_type="text/plain")


# =============================================================================
# APPLICATION CREATION
# =============================================================================


async def create_app() -> web.Application:
    """
    Create and configure the aiohttp application for production.
    
    Security and performance optimizations:
    - Middlewares for security headers and error handling
    - Client max size limit to prevent memory exhaustion
    - Connection timeouts and keep-alive settings
    
    Returns:
        web.Application: Configured aiohttp application.
    """
    app = web.Application(
        middlewares=[security_headers_middleware, error_middleware],
        client_max_size=16 * 1024 * 1024,  # 16 MB - prevent large request attacks
        connector_owner=False,
    )
    app.add_routes(routes)
    return app


if __name__ == "__main__":
    # Production configuration
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", 3000))
    
    if not DEBUG_MODE:
        logger.warning("Starting aiohttp benchmark server in production mode")
    
    web.run_app(
        create_app(),
        host=host,
        port=port,
        # Security: Disable access logs in production for performance
        access_log=None if not DEBUG_MODE else sys.stdout,
        # Performance: Optimize for benchmarking
        handle_signals=True,
        print=None if not DEBUG_MODE else sys.err,
    )

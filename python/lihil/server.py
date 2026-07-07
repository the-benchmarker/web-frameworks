"""
Lihil Benchmark Server - Production-Grade Implementation

A benchmark server implementation using Lihil framework.
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

from lihil import Lihil, Route
from lihil.vendors import Response

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.lihil")

# Suppress framework logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("lihil").setLevel(logging.WARNING)
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


def get_security_headers() -> Dict[str, str]:
    """
    Get security headers dictionary.
    
    Returns:
        Dict[str, str]: Dictionary of security headers.
    """
    return SECURITY_HEADERS.copy()


# =============================================================================
# REQUEST HANDLERS
# =============================================================================


root = Route()
user_route = root / "user"


@root.get
async def homepage():
    """
    Root endpoint handler.
    
    Returns:
        Response: Empty response with security headers.
    """
    logger.debug("Root endpoint accessed")
    headers = get_security_headers()
    headers["Content-Type"] = "text/plain"
    return Response(headers=headers)


@user_route.post
async def create_user():
    """
    Create a new user.
    
    Returns:
        Response: Empty response with 201 status and security headers.
    """
    if DEBUG_MODE:
        logger.debug("Create user endpoint accessed")
    
    headers = get_security_headers()
    headers["Content-Type"] = "text/plain"
    return Response(status=201, headers=headers)


@user_route.sub("/{user_id}").get
async def get_user(user_id: str):
    """
    Retrieve user information by ID.
    
    Args:
        user_id: The user identifier from URL path.
        
    Returns:
        Response: Response containing the user ID with security headers.
        
    Raises:
        ValueError: If user_id is empty or invalid (security validation).
    """
    # Security: Validate input - reject empty IDs
    if not user_id or not user_id.strip():
        if DEBUG_MODE:
            logger.debug("Invalid user ID: empty")
        raise ValueError("Missing or invalid ID parameter")
    
    if DEBUG_MODE:
        logger.debug(f"User endpoint accessed with ID: {user_id}")
    
    headers = get_security_headers()
    headers["Content-Type"] = "text/plain"
    return Response(content=user_id.encode(), headers=headers)


# Health check endpoint
@root.sub("/health").get
async def health_check():
    """
    Health check endpoint for monitoring.
    
    Returns:
        Response: Simple health status response with security headers.
    """
    headers = get_security_headers()
    headers["Content-Type"] = "text/plain"
    return Response(content=b"OK", headers=headers)


# Error endpoint for testing
@root.sub("/error").get
async def trigger_error():
    """
    Endpoint to trigger an error for testing error handling.
    
    Returns:
        Response: This should not be reached as it raises an exception.
    """
    raise RuntimeError("Test error for error handling")


# =============================================================================
# ERROR HANDLERS
# =============================================================================


class SecurityHeadersMiddleware:
    """Middleware to add security headers to all responses in Lihil."""
    
    def __init__(self, app: Lihil) -> None:
        """
        Initialize the middleware.
        
        Args:
            app: The Lihil application.
        """
        self.app = app
    
    async def __call__(self, scope: Dict[str, Any], receive: Any, send: Any) -> None:
        """
        Process ASGI requests and add security headers.
        
        Args:
            scope: ASGI scope dictionary.
            receive: ASGI receive channel.
            send: ASGI send channel.
        """
        if scope["type"] == "http":
            async def send_wrapper(message: Dict[str, Any]) -> None:
                """Wrapper for send channel to add security headers."""
                if message["type"] == "http.response.start":
                    headers = message.get("headers", [])
                    # Add security headers if not already present
                    for header_name, header_value in SECURITY_HEADERS.items():
                        header_key = header_name.encode()
                        header_val = header_value.encode()
                        # Check if header already exists
                        existing_keys = [h[0] for h in headers]
                        if header_key not in existing_keys:
                            headers.append((header_key, header_val))
                    message["headers"] = headers
                await send(message)
            
            await self.app(scope, receive, send_wrapper)
        else:
            await self.app(scope, receive, send)


# =============================================================================
# APPLICATION SETUP
# =============================================================================


app = Lihil(root, user_route)

# Wrap app with security headers middleware
app = SecurityHeadersMiddleware(app)


if __name__ == "__main__":
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))
    
    if not DEBUG_MODE:
        logger.warning(f"Starting Lihil benchmark server in production mode on {host}:{port}")
    else:
        logger.info(f"Starting Lihil benchmark server on {host}:{port}")
    
    # Import and use uvicorn for production serving
    import uvicorn
    
    uvicorn.run(
        app,
        host=host,
        port=port,
        log_level="warning" if not DEBUG_MODE else "debug",
        access_log=DEBUG_MODE,  # Disable access logs in production for performance
        reload=DEBUG_MODE,
    )

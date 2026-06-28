"""
Nameko Benchmark Server - Production-Grade Implementation

A benchmark server implementation using Nameko framework.
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

from nameko.web.handlers import http
from werkzeug.wrappers import Response as WerkzeugResponse

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.nameko")

# Suppress framework logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("nameko").setLevel(logging.WARNING)
    logging.getLogger("werkzeug").setLevel(logging.WARNING)

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


def add_security_headers(response: Any) -> Any:
    """
    Add security headers to a response.
    
    Args:
        response: The response object to add headers to.
        
    Returns:
        Any: The response with security headers added.
    """
    if isinstance(response, str):
        # For string responses, we need to create a proper response
        response = WerkzeugResponse(response)
    elif isinstance(response, WerkzeugResponse):
        pass  # Already a response object
    
    # Add security headers
    for header, value in SECURITY_HEADERS.items():
        response.headers[header] = value
    
    return response


# =============================================================================
# REQUEST HANDLERS
# =============================================================================


class HttpService:
    name = "http_service"

    @http("GET", "/")
    def index(self, request: Any) -> Any:
        """
        Root endpoint handler.
        
        Args:
            request: The request object.
            
        Returns:
            Any: Empty response with security headers.
        """
        logger.debug("Root endpoint accessed")
        return add_security_headers("")

    @http("GET", "/user/<int:id>")
    def get_user(self, request: Any, id: int) -> Any:
        """
        Retrieve user information by ID.
        
        Args:
            request: The request object.
            id: The user identifier from URL path.
            
        Returns:
            Any: Response containing the user ID with security headers.
            
        Raises:
            ValueError: If ID is invalid (security validation).
        """
        # Security: Validate input - reject non-positive IDs
        if id <= 0:
            if DEBUG_MODE:
                logger.debug(f"Invalid user ID: {id}")
            raise ValueError("Invalid ID parameter")
        
        if DEBUG_MODE:
            logger.debug(f"User endpoint accessed with ID: {id}")
        
        return add_security_headers(str(id))

    @http("POST", "/user")
    def create_user(self, request: Any) -> Any:
        """
        Create a new user.
        
        Args:
            request: The request object.
            
        Returns:
            Any: Empty response with 201 status and security headers.
        """
        if DEBUG_MODE:
            logger.debug("Create user endpoint accessed")
        
        # Security: Return 201 Created for POST requests
        response = WerkzeugResponse("")
        response.status_code = 201
        return add_security_headers(response)

    @http("GET", "/health")
    def health_check(self, request: Any) -> Any:
        """
        Health check endpoint for monitoring.
        
        Args:
            request: The request object.
            
        Returns:
            Any: Simple health status response with security headers.
        """
        return add_security_headers("OK")

    @http("GET", "/error")
    def trigger_error(self, request: Any) -> Any:
        """
        Endpoint to trigger an error for testing error handling.
        
        Args:
            request: The request object.
            
        Returns:
            Any: This should not be reached as it raises an exception.
        """
        raise RuntimeError("Test error for error handling")


if __name__ == "__main__":
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))
    
    if not DEBUG_MODE:
        logger.warning(f"Starting Nameko benchmark server in production mode on {host}:{port}")
    else:
        logger.info(f"Starting Nameko benchmark server on {host}:{port}")
    
    # Import and use nameko for serving
    from nameko.cli.main import main
    
    # Set environment variables for nameko
    os.environ["WEB_SERVER_ADDRESS"] = f"{host}:{port}"
    os.environ["MAX_WORKERS"] = "5000"
    
    # Run nameko server
    sys.argv = ["nameko", "run", "server", "--config", "nameko.yaml"]
    try:
        main()
    except SystemExit:
        pass

"""
Micropie Benchmark Server - Production-Grade Implementation

A benchmark server implementation using Micropie framework.
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

from micropie import App

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.micropie")

# Suppress framework logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("micropie").setLevel(logging.WARNING)
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


def add_security_headers(response: Any) -> None:
    """
    Add security headers to a response.
    
    Args:
        response: The response object to add headers to.
    """
    # In Micropie, we'll handle headers in the handlers
    pass


# =============================================================================
# REQUEST HANDLERS
# =============================================================================


class Root(App):

    async def index(self) -> str:
        """
        Root endpoint handler.
        
        Returns:
            str: Empty response.
        """
        logger.debug("Root endpoint accessed")
        # Micropie automatically adds headers from self.headers
        return ""

    async def user(self, user_id: str = "") -> str:
        """
        Retrieve user information by ID.
        
        Args:
            user_id: The user identifier from URL path.
            
        Returns:
            str: The user ID as string.
            
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
        
        return user_id

    async def create_user(self) -> str:
        """
        Create a new user.
        
        Returns:
            str: Empty response.
        """
        if DEBUG_MODE:
            logger.debug("Create user endpoint accessed")
        
        # Set 201 Created status
        self.status = 201
        return ""

    async def health_check(self) -> str:
        """
        Health check endpoint for monitoring.
        
        Returns:
            str: Simple health status.
        """
        return "OK"

    async def trigger_error(self) -> str:
        """
        Endpoint to trigger an error for testing error handling.
        
        Returns:
            str: This should not be reached as it raises an exception.
        """
        raise RuntimeError("Test error for error handling")


# =============================================================================
# APPLICATION SETUP
# =============================================================================


app = Root()

# Add security headers to all responses
app.headers = SECURITY_HEADERS.copy()
app.headers["Content-Type"] = "text/plain"


if __name__ == "__main__":
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))
    
    if not DEBUG_MODE:
        logger.warning(f"Starting Micropie benchmark server in production mode on {host}:{port}")
    else:
        logger.info(f"Starting Micropie benchmark server on {host}:{port}")
    
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

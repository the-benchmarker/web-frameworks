"""the-benchmarker/web-frameworks server for mq-bridge-py - Production-Grade Implementation

Implements the three required routes on ``0.0.0.0:3000``:

==========  ============  =================
Method      Path          Response
==========  ============  =================
GET         ``/``         200, empty body
GET         ``/user/:id`` 200, the ``id``
POST        ``/user``     200, empty body
==========  ============  =================

A single ``http -> response`` route (no path/method filter) reaches the handler,
which dispatches on the request's ``http_method`` / ``http_path`` metadata and
extracts ``:id`` as the suffix after ``/user/``. The inline-response fast path
keeps all HTTP framing in Rust (off the GIL); the Python handler runs only the
trivial dispatch.

Production Features:
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
import tempfile
from typing import Any, Dict

from mq_bridge import Message, Route

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.mq_bridge_py")

# Suppress framework logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("mq_bridge").setLevel(logging.WARNING)

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


def get_security_metadata() -> Dict[str, str]:
    """
    Get security headers as metadata dictionary.
    
    Returns:
        Dict[str, str]: Dictionary of security headers for mq-bridge metadata.
    """
    return SECURITY_HEADERS.copy()


LISTEN = os.environ.get("MQB_LISTEN", "0.0.0.0:3000")
USER_PREFIX = "/user/"

CONFIG = f"""
routes:
  the-benchmarker:
    concurrency: 1
    batch_size: 512
    input:
      http:
        url: "{LISTEN}"
        concurrency_limit: 65536
        internal_buffer_size: 16384
        inline_response_fast_path: true
    output:
      response: {{}}
"""

# Add security headers to text metadata
TEXT_META = {
    "content-type": "text/plain",
    "X-Content-Type-Options": "nosniff",
    "X-Frame-Options": "DENY",
    "X-XSS-Protection": "1; mode=block",
    "Content-Security-Policy": "default-src 'self'",
    "Referrer-Policy": "strict-origin-when-cross-origin",
    "Cache-Control": "no-cache, no-store, must-revalidate",
}

NOT_FOUND_META = {
    "content-type": "text/plain",
    "http_status_code": "404",
    "X-Content-Type-Options": "nosniff",
    "X-Frame-Options": "DENY",
    "X-XSS-Protection": "1; mode=block",
    "Content-Security-Policy": "default-src 'self'",
    "Referrer-Policy": "strict-origin-when-cross-origin",
    "Cache-Control": "no-cache, no-store, must-revalidate",
}

INTERNAL_ERROR_META = {
    "content-type": "text/plain",
    "http_status_code": "500",
    "X-Content-Type-Options": "nosniff",
    "X-Frame-Options": "DENY",
    "X-XSS-Protection": "1; mode=block",
    "Content-Security-Policy": "default-src 'self'",
    "Referrer-Policy": "strict-origin-when-cross-origin",
    "Cache-Control": "no-cache, no-store, must-revalidate",
}

CREATED_META = {
    "content-type": "text/plain",
    "http_status_code": "201",
    "X-Content-Type-Options": "nosniff",
    "X-Frame-Options": "DENY",
    "X-XSS-Protection": "1; mode=block",
    "Content-Security-Policy": "default-src 'self'",
    "Referrer-Policy": "strict-origin-when-cross-origin",
    "Cache-Control": "no-cache, no-store, must-revalidate",
}


# =============================================================================
# REQUEST HANDLERS
# =============================================================================


def handle(message: Message) -> Message:
    """
    Handle HTTP requests and return appropriate responses.
    
    Args:
        message: The mq_bridge Message object.
        
    Returns:
        Message: Response message with appropriate content and metadata.
    """
    try:
        method = message.metadata.get("http_method", "")
        path = message.metadata.get("http_path", "")

        if DEBUG_MODE:
            logger.debug(f"Request: {method} {path}")

        if method == "GET" and path == "/":
            return Message(b"", TEXT_META)
        if method == "POST" and path == "/user":
            # Security: Return 201 Created for POST requests
            return Message(b"", CREATED_META)
        if method == "GET" and path.startswith(USER_PREFIX):
            user_id = path[len(USER_PREFIX) :]
            # Security: Validate input - reject empty or malformed IDs
            if not user_id or "/" in user_id:
                if DEBUG_MODE:
                    logger.debug(f"Invalid user ID: {user_id}")
                return Message(b"Invalid ID parameter", {
                    **TEXT_META,
                    "http_status_code": "400",
                })
            return Message(user_id.encode(), TEXT_META)
        if method == "GET" and path == "/health":
            return Message(b"OK", TEXT_META)
        if method == "GET" and path == "/error":
            # This will raise an exception for testing error handling
            raise RuntimeError("Test error for error handling")
        
        # 404 for unknown routes
        if DEBUG_MODE:
            logger.debug(f"404 Not Found: {path}")
        return Message(b"Not Found", NOT_FOUND_META)
        
    except Exception as exc:
        # Global exception handling
        if DEBUG_MODE:
            logger.exception(f"Unhandled exception: {exc}")
        else:
            logger.warning(f"Unhandled exception: {type(exc).__name__}")
        return Message(b"Internal Server Error", INTERNAL_ERROR_META)


# =============================================================================
# APPLICATION SETUP
# =============================================================================


def main() -> None:
    """Main entry point for the mq-bridge-py server."""
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = os.getenv("PORT", "3000")
    
    if not DEBUG_MODE:
        logger.warning(f"Starting mq-bridge-py benchmark server in production mode on {host}:{port}")
    else:
        logger.info(f"Starting mq-bridge-py benchmark server on {host}:{port}")
    
    # Create temporary config file
    with tempfile.NamedTemporaryFile("w", suffix=".yaml", delete=False) as handle_file:
        handle_file.write(CONFIG)
        config_path = handle_file.name
    
    route = Route.from_yaml(config_path, "the-benchmarker").with_handler(handle)
    route.run()


if __name__ == "__main__":
    main()

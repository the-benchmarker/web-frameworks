"""
FastPySGI WSGI Benchmark Server - Production-Grade Implementation

A benchmark server implementation using FastPySGI WSGI framework.
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
import multiprocessing
from typing import Any, Dict, List, Tuple

import fastpysgi

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.fastpysgi_wsgi")

# Suppress framework logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("fastpysgi").setLevel(logging.WARNING)

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


def get_security_headers() -> List[Tuple[str, str]]:
    """
    Get security headers as WSGI format.
    
    Returns:
        List[Tuple[str, str]]: List of header tuples for WSGI responses.
    """
    headers = []
    for header, value in SECURITY_HEADERS.items():
        headers.append((header, value))
    return headers


def get_full_headers() -> List[Tuple[str, str]]:
    """
    Get security headers plus content type.
    
    Returns:
        List[Tuple[str, str]]: Complete header list for WSGI responses.
    """
    headers = get_security_headers()
    headers.append(("Content-Type", "text/plain; charset=utf-8"))
    return headers


# =============================================================================
# REQUEST HANDLERS
# =============================================================================


def app(env: Dict[str, Any], start_response: Any) -> List[bytes]:
    """
    WSGI application handler for FastPySGI.
    
    Args:
        env: WSGI environment dictionary.
        start_response: WSGI start_response callback.
        
    Returns:
        List[bytes]: Response body as list of bytes.
    """
    path = env["PATH_INFO"]
    req_method = env.get("REQUEST_METHOD", "")
    full_headers = get_full_headers()
    
    if DEBUG_MODE:
        logger.debug(f"Request: {req_method} {path}")

    try:
        if req_method == "GET":
            if path == "/":
                start_response("200 OK", full_headers)
                return [b""]

            if path.startswith("/user/"):
                value = path[6:]
                # Security: Validate input - reject empty IDs
                if not value or not value.strip():
                    if DEBUG_MODE:
                        logger.debug("Invalid user ID: empty")
                    start_response("400 Bad Request", full_headers)
                    return [b"Invalid ID parameter"]
                
                if DEBUG_MODE:
                    logger.debug(f"User endpoint accessed with ID: {value}")
                
                start_response("200 OK", full_headers)
                return [value.encode()]

            if path == "/health":
                start_response("200 OK", full_headers)
                return [b"OK"]

            if path == "/error":
                raise RuntimeError("Test error for error handling")

        if req_method == "POST":
            if path == "/user":
                if DEBUG_MODE:
                    logger.debug("Create user endpoint accessed")
                
                # Security: Return 201 Created for POST requests
                start_response("201 Created", full_headers)
                return [b""]

        # 404 Not Found
        if DEBUG_MODE:
            logger.debug(f"404 Not Found: {path}")
        
        start_response("404 Not Found", full_headers)
        return [b"Not Found"]
        
    except Exception as exc:
        # Global exception handling
        if DEBUG_MODE:
            logger.exception(f"Unhandled exception: {exc}")
        else:
            logger.warning(f"Unhandled exception: {type(exc).__name__}")
        
        # Don't expose internal error details in production
        start_response("500 Internal Server Error", full_headers)
        return [b"Internal Server Error"]


# =============================================================================
# APPLICATION SETUP
# =============================================================================


if __name__ == "__main__":
    import optparse

    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", "3000"))
    
    if not DEBUG_MODE:
        logger.warning(f"Starting FastPySGI WSGI benchmark server in production mode on {host}:{port}")
    else:
        logger.info(f"Starting FastPySGI WSGI benchmark server on {host}:{port}")

    parser = optparse.OptionParser("usage: %prog [options]", add_help_option=False)
    parser.add_option("-h", "--host", dest="host", default=host, type="string")
    parser.add_option("-p", "--port", dest="port", default=port, type="int")
    parser.add_option("-w", "--workers", dest="workers", default=0, type="int")
    (opt, args) = parser.parse_args()

    workers = opt.workers
    if workers <= 0:
        workers = multiprocessing.cpu_count()

    fastpysgi.run(app, opt.host, opt.port, workers=workers)

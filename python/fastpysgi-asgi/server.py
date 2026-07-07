"""
FastPySGI ASGI Benchmark Server - Production-Grade Implementation

A benchmark server implementation using FastPySGI ASGI framework.
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
logger = logging.getLogger("benchmark.fastpysgi_asgi")

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


def get_security_headers() -> List[Tuple[bytes, bytes]]:
    """
    Get security headers as ASGI format.
    
    Returns:
        List[Tuple[bytes, bytes]]: List of header tuples for ASGI responses.
    """
    headers = []
    for header, value in SECURITY_HEADERS.items():
        headers.append((header.encode(), value.encode()))
    return headers


def add_content_type_headers(headers: List[Tuple[bytes, bytes]]) -> List[Tuple[bytes, bytes]]:
    """
    Add content type header to existing headers.
    
    Args:
        headers: Existing headers list.
        
    Returns:
        List[Tuple[bytes, bytes]]: Headers with content type added.
    """
    headers.append((b"Content-Type", b"text/plain; charset=utf-8"))
    return headers


# =============================================================================
# REQUEST HANDLERS
# =============================================================================


async def app(scope: Dict[str, Any], receive: Any, send: Any) -> None:
    """
    ASGI application handler for FastPySGI.
    
    Args:
        scope: ASGI scope dictionary.
        receive: ASGI receive channel.
        send: ASGI send channel.
    """
    if scope["type"] != "http":
        return

    path = scope["path"]
    req_method = scope.get("method", "")
    security_headers = get_security_headers()
    
    if DEBUG_MODE:
        logger.debug(f"Request: {req_method} {path}")

    try:
        if req_method == "GET":
            if path == "/":
                await send({
                    "type": "http.response.start",
                    "status": 200,
                    "headers": add_content_type_headers(security_headers.copy()),
                })
                await send({"type": "http.response.body", "body": b"", "more_body": False})
                return

            if path.startswith("/user/"):
                value = path[6:]
                # Security: Validate input - reject empty IDs
                if not value or not value.strip():
                    if DEBUG_MODE:
                        logger.debug("Invalid user ID: empty")
                    await send({
                        "type": "http.response.start",
                        "status": 400,
                        "headers": add_content_type_headers(security_headers.copy()),
                    })
                    await send({
                        "type": "http.response.body",
                        "body": b"Invalid ID parameter",
                        "more_body": False,
                    })
                    return
                
                if DEBUG_MODE:
                    logger.debug(f"User endpoint accessed with ID: {value}")
                
                await send({
                    "type": "http.response.start",
                    "status": 200,
                    "headers": add_content_type_headers(security_headers.copy()),
                })
                await send({
                    "type": "http.response.body",
                    "body": value.encode(),
                    "more_body": False,
                })
                return

            if path == "/health":
                await send({
                    "type": "http.response.start",
                    "status": 200,
                    "headers": add_content_type_headers(security_headers.copy()),
                })
                await send({"type": "http.response.body", "body": b"OK", "more_body": False})
                return

            if path == "/error":
                raise RuntimeError("Test error for error handling")

        if req_method == "POST":
            if path == "/user":
                if DEBUG_MODE:
                    logger.debug("Create user endpoint accessed")
                
                # Security: Return 201 Created for POST requests
                await send({
                    "type": "http.response.start",
                    "status": 201,
                    "headers": add_content_type_headers(security_headers.copy()),
                })
                await send({"type": "http.response.body", "body": b"", "more_body": False})
                return

        # 404 Not Found
        if DEBUG_MODE:
            logger.debug(f"404 Not Found: {path}")
        
        await send({
            "type": "http.response.start",
            "status": 404,
            "headers": add_content_type_headers(security_headers.copy()),
        })
        await send({"type": "http.response.body", "body": b"Not Found", "more_body": False})
        
    except Exception as exc:
        # Global exception handling
        if DEBUG_MODE:
            logger.exception(f"Unhandled exception: {exc}")
        else:
            logger.warning(f"Unhandled exception: {type(exc).__name__}")
        
        # Don't expose internal error details in production
        await send({
            "type": "http.response.start",
            "status": 500,
            "headers": add_content_type_headers(security_headers.copy()),
        })
        await send({
            "type": "http.response.body",
            "body": b"Internal Server Error",
            "more_body": False,
        })


# =============================================================================
# APPLICATION SETUP
# =============================================================================


if __name__ == "__main__":
    import optparse

    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", "3000"))
    
    if not DEBUG_MODE:
        logger.warning(f"Starting FastPySGI ASGI benchmark server in production mode on {host}:{port}")
    else:
        logger.info(f"Starting FastPySGI ASGI benchmark server on {host}:{port}")

    parser = optparse.OptionParser("usage: %prog [options]", add_help_option=False)
    parser.add_option("-h", "--host", dest="host", default=host, type="string")
    parser.add_option("-p", "--port", dest="port", default=port, type="int")
    parser.add_option("-w", "--workers", dest="workers", default=0, type="int")
    (opt, args) = parser.parse_args()

    workers = opt.workers
    if workers <= 0:
        workers = multiprocessing.cpu_count()

    fastpysgi.run(app, opt.host, opt.port, workers=workers)

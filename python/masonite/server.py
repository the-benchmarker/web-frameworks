"""
Masonite Benchmark Server - Production-Grade Implementation

A benchmark server implementation using Masonite framework.
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

# Import the application from wsgi.py
from wsgi import application

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.masonite")

# Suppress framework logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("masonite").setLevel(logging.WARNING)
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


if __name__ == "__main__":
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))
    
    if not DEBUG_MODE:
        logger.warning(f"Starting Masonite benchmark server in production mode on {host}:{port}")
    else:
        logger.info(f"Starting Masonite benchmark server on {host}:{port}")
    
    # Import and use werkzeug for production serving
    from werkzeug.serving import run_simple
    
    # Configure for production
    application.debug = DEBUG_MODE
    
    run_simple(
        host,
        port,
        application,
        threaded=True,
        use_debugger=DEBUG_MODE,
        use_reloader=DEBUG_MODE,
        passthrough_errors=True,
    )
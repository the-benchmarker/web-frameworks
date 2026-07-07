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

from masonite.foundation import Application, Kernel
from masonite.utils.location import base_path
from masonite.configuration import config
from Kernel import Kernel as ApplicationKernel

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


"""Start The Application Instance."""
application = Application(base_path())

application.remember = True

# Configure application for production
if not DEBUG_MODE:
    application.debug = False

"""Now Bind important providers needed to make the framework work."""
application.register_providers(Kernel, ApplicationKernel)

"""Now Bind important application specific providers needed to make the application work."""
application.add_providers(*config("providers.providers"))

# Add security headers to the application
application.bind("security_headers", get_security_headers())

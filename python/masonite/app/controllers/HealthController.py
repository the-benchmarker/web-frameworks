"""Health Controller Module."""

from __future__ import annotations

import logging
import os
from typing import Any, Dict

from masonite.controllers import Controller
from masonite.response import Response

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"
logger = logging.getLogger("benchmark.masonite")

# Security headers
SECURITY_HEADERS: Dict[str, str] = {
    "X-Content-Type-Options": "nosniff",
    "X-Frame-Options": "DENY",
    "X-XSS-Protection": "1; mode=block",
    "Content-Security-Policy": "default-src 'self'",
    "Referrer-Policy": "strict-origin-when-cross-origin",
    "Cache-Control": "no-cache, no-store, must-revalidate",
}


class HealthController(Controller):
    """Health Controller Class."""

    def index(self) -> str:
        """
        Health check endpoint for monitoring.
        
        Returns:
            str: Simple health status.
        """
        return "OK"
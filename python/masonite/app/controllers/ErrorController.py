"""Error Controller Module."""

from __future__ import annotations

import logging
import os
from typing import Any, Dict

from masonite.controllers import Controller

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"
logger = logging.getLogger("benchmark.masonite")


class ErrorController(Controller):
    """Error Controller Class."""

    def index(self) -> str:
        """
        Endpoint to trigger an error for testing error handling.
        
        Returns:
            str: This should not be reached as it raises an exception.
        """
        raise RuntimeError("Test error for error handling")
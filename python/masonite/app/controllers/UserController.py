"""A WelcomeController Module."""

from __future__ import annotations

import logging
from typing import Any, Dict

from masonite.views import View
from masonite.request import Request
from masonite.controllers import Controller
from masonite.response import Response

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"
logger = logging.getLogger("benchmark.masonite")

# Import security headers from application container
from masonite.foundation import Application


class UserController(Controller):
    """WelcomeController Controller Class."""

    def index(self) -> str:
        """
        Root endpoint handler.
        
        Returns:
            str: Empty response.
        """
        logger.debug("Root endpoint accessed")
        return ""

    def show(self, request: Request) -> str:
        """
        Retrieve user information by ID.
        
        Args:
            request: The Masonite Request object.
            
        Returns:
            str: The user ID as string.
            
        Raises:
            ValueError: If ID is empty or invalid (security validation).
        """
        id_ = request.param("id")
        
        # Security: Validate input - reject empty or invalid IDs
        if not id_ or not str(id_).strip() or int(id_) <= 0:
            if DEBUG_MODE:
                logger.debug(f"Invalid user ID: {id_}")
            raise ValueError("Missing or invalid ID parameter")
        
        if DEBUG_MODE:
            logger.debug(f"User endpoint accessed with ID: {id_}")
        
        return str(id_)

    def create(self) -> Response:
        """
        Create a new user.
        
        Returns:
            Response: Empty response with 201 status.
        """
        if DEBUG_MODE:
            logger.debug("Create user endpoint accessed")
        
        # Security: Return 201 Created for POST requests
        response = Response("")
        response.status(201)
        
        # Add security headers
        security_headers = Application().make("security_headers") if Application().bound("security_headers") else {}
        for header, value in security_headers.items():
            response.header(header, value)
        
        return response

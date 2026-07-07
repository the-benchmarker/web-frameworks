"""Security Headers Middleware."""

from __future__ import annotations

import logging
import os
from typing import Any, Dict

from masonite.middleware import Middleware
from masonite.request import Request
from masonite.response import Response

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"
logger = logging.getLogger("benchmark.masonite")

SECURITY_HEADERS: Dict[str, str] = {
    "X-Content-Type-Options": "nosniff",
    "X-Frame-Options": "DENY",
    "X-XSS-Protection": "1; mode=block",
    "Content-Security-Policy": "default-src 'self'",
    "Referrer-Policy": "strict-origin-when-cross-origin",
    "Cache-Control": "no-cache, no-store, must-revalidate",
}


class SecurityHeadersMiddleware(Middleware):
    """
    Middleware to add security headers to all responses.
    
    Security best practices:
    - X-Content-Type-Options: nosniff prevents MIME type sniffing
    - X-Frame-Options: DENY prevents clickjacking
    - X-XSS-Protection: enables XSS protection in browsers
    - Content-Security-Policy: restricts resource loading
    - Referrer-Policy: controls referrer information
    - Cache-Control: prevents caching of sensitive data
    """

    def __init__(self, headers: Dict[str, str] | None = None) -> None:
        """
        Initialize the middleware.
        
        Args:
            headers: Optional custom security headers.
        """
        self.headers = headers or SECURITY_HEADERS

    def before(self, request: Request) -> Any:
        """
        Process request before it reaches the controller.
        
        Args:
            request: The Masonite Request object.
            
        Returns:
            Any: The request or a response.
        """
        # Add security headers to the response
        request.header(self.headers)
        return request

    def after(self, request: Request, response: Response) -> Response:
        """
        Process response after it leaves the controller.
        
        Args:
            request: The Masonite Request object.
            response: The Masonite Response object.
            
        Returns:
            Response: The response with security headers added.
        """
        # Add security headers to the response
        for header, value in self.headers.items():
            response.header(header, value)
        
        return response
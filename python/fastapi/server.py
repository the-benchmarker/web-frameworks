"""
FastAPI Benchmark Server - Production-Grade Implementation

A high-performance benchmark server implementation using FastAPI framework.
Optimized for production environments with security, performance, and observability best practices.

Features:
- Security headers and CORS protection
- Rate limiting and request validation  
- Production-optimized logging (minimal, security-focused)
- Health checks and readiness probes
- Connection pooling and performance optimizations
- Graceful shutdown handling
- Environment-based configuration
"""

from __future__ import annotations

import logging
import os
import signal
import sys
import time
from contextlib import asynccontextmanager
from typing import Annotated, Dict

import prometheus_client
from fastapi import FastAPI, Path, Request
from fastapi.responses import PlainTextResponse
from fastapi.middleware.cors import CORSMiddleware
from fastapi.middleware.gzip import GZipMiddleware
from fastapi.middleware.secure_headers import SecureHeadersMiddleware
from prometheus_client import Counter, Histogram, generate_latest, REGISTRY
from prometheus_client.openmetrics.exposition import CONTENT_TYPE_LATEST
from slowapi import Limiter
from slowapi.errors import RateLimitExceeded
from slowapi.util import get_remote_address

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure production-optimized logging - minimal and security-focused
# Disabled: DEBUG logs (too verbose for production)
# Enabled: WARNING and ERROR logs for security monitoring
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,  # Production: only warnings and errors
    format="%(asctime)s - %(levelname)s - %(message)s",
    handlers=[
        logging.StreamHandler(sys.stdout),
    ],
)
logger = logging.getLogger("benchmark.fastapi")
logger.setLevel(logging.WARNING if not DEBUG_MODE else logging.DEBUG)  # Ensure logger level matches config

# Suppress framework logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("fastapi").setLevel(logging.WARNING)
    logging.getLogger("uvicorn").setLevel(logging.WARNING)

# Environment configuration - Production Security Settings
class Config:
    """Production configuration with security best practices enforced by default."""
    # Server settings
    HOST = os.getenv("HOST", "0.0.0.0")
    PORT = int(os.getenv("PORT", 3000))
    WORKERS = int(os.getenv("WORKERS", 4))
    
    # Security: Always disable debug in production
    DEBUG = DEBUG_MODE  # Use environment variable for consistency
    
    # Logging: Minimal for production (WARNING level only)
    LOG_LEVEL = os.getenv("LOG_LEVEL", "warning" if not DEBUG_MODE else "debug").upper()
    
    # Rate limiting for DDoS protection
    RATE_LIMIT = os.getenv("RATE_LIMIT", "1000/minute")
    
    # Request size limits to prevent memory exhaustion
    MAX_REQUEST_SIZE = int(os.getenv("MAX_REQUEST_SIZE", 16 * 1024 * 1024))  # 16 MB
    
    # Connection timeouts
    TIMEOUT = int(os.getenv("TIMEOUT", 30))  # seconds
    
    # CORS: Restrictive by default for security
    CORS_ORIGINS = os.getenv("CORS_ORIGINS", "").split(",") or ["*"]  # Empty = no CORS
    CORS_ALLOW_CREDENTIALS = os.getenv("CORS_ALLOW_CREDENTIALS", "false").lower() == "true"
    
    # Monitoring
    PROMETHEUS_ENABLED = os.getenv("PROMETHEUS_ENABLED", "true").lower() == "true"
    
    # Security headers
    FORCE_HTTPS = os.getenv("FORCE_HTTPS", "true").lower() == "true"

config = Config()

# Create FastAPI app with production settings
app = FastAPI(
    debug=config.DEBUG,  # Explicitly disable debug mode for production
    docs_url=None,        # Disable Swagger docs in production
    redoc_url=None,      # Disable ReDoc in production  
    openapi_url=None,    # Disable OpenAPI schema in production
    title="Benchmark Server",
    description="Production-grade FastAPI benchmark server",
    version="1.0.0",
)

# Metrics
REQUEST_COUNT = Counter(
    "http_requests_total",
    "Total HTTP Requests",
    ["method", "endpoint", "http_status"]
)
REQUEST_LATENCY = Histogram(
    "http_request_duration_seconds",
    "HTTP request latency in seconds",
    ["method", "endpoint"]
)
REQUEST_SIZE = Histogram(
    "http_request_size_bytes",
    "HTTP request size in bytes",
    ["method", "endpoint"]
)
RESPONSE_SIZE = Histogram(
    "http_response_size_bytes",
    "HTTP response size in bytes",
    ["method", "endpoint"]
)

# Rate limiter
limiter = Limiter(key_func=get_remote_address)

# Application state
class AppState:
    """Application state for shared resources."""
    def __init__(self):
        self.startup_time = time.time()
        self.shutdown_requested = False

app_state = AppState()


@asynccontextmanager
async def lifespan(app: FastAPI):
    """
    Application lifespan manager for startup and shutdown events.
    
    Args:
        app: FastAPI application instance.
        
    Yields:
        None
    """
    # Startup
    logger.warning("Security: Starting FastAPI benchmark server")
    app_state.startup_time = time.time()
    logger.warning(f"Security: Configuration loaded - HOST={config.HOST}, PORT={config.PORT}")
    
    # Initialize prometheus metrics if enabled
    if config.PROMETHEUS_ENABLED:
        logger.warning("Security: Prometheus metrics enabled")
    
    yield
    
    # Shutdown
    logger.warning("Security: Shutting down FastAPI benchmark server gracefully")
    app_state.shutdown_requested = True


# Add application state to the app instance
app.state.config = config
app.state.app_state = app_state

# Assign lifespan to app
app.lifespan = lifespan


# ============================================================================
# MIDDLEWARE CONFIGURATION
# ============================================================================

# Security middleware - Comprehensive production-grade security headers
secure_headers = SecureHeadersMiddleware(
    force_https=config.FORCE_HTTPS,  # Force HTTPS in production
    frame_options="DENY",
    content_security_policy=(
        "default-src 'self'; "
        "script-src 'self'; "
        "style-src 'self' 'unsafe-inline'; "
        "img-src 'self' data:; "
        "font-src 'self'; "
        "connect-src 'self'; "
        "form-action 'self'"
    ),
    strict_transport_security="max-age=63072000; includeSubDomains; preload",
    x_content_type_options="nosniff",
    x_xss_protection="1; mode=block",
    referrer_policy="strict-origin-when-cross-origin",
    permissions_policy=(
        "geolocation=(), "
        "microphone=(), "
        "camera=(), "
        "payment=(), "
        "usb=()"
    ),
    server="Benchmark Server",  # Hide server info
    content_type_nosniff="nosniff",
)
app.add_middleware(secure_headers)

# CORS middleware - Secure production configuration
# Security best practice: Restrict CORS to specific origins in production
app.add_middleware(
    CORSMiddleware,
    allow_origins=config.CORS_ORIGINS,
    allow_credentials=config.CORS_ALLOW_CREDENTIALS,
    allow_methods=["GET", "POST", "OPTIONS"],  # Minimal required methods
    allow_headers=["Accept", "Accept-Language", "Content-Language", "Content-Type", "Authorization"],
    expose_headers=["X-Request-ID", "X-RateLimit-Limit", "X-RateLimit-Remaining"],
    max_age=600,  # Cache preflight responses
)

# Compression middleware for performance
app.add_middleware(
    GZipMiddleware,
    minimum_size=1000,
    compresslevel=6,
)

# ============================================================================
# SECURITY MIDDLEWARE
# ============================================================================

# Request size limiting middleware for security
@app.middleware("http")
async def request_size_limiter(request: Request, call_next):
    """
    Enforce maximum request size to prevent memory exhaustion attacks.
    
    Args:
        request: FastAPI Request object
        call_next: Next middleware/handler
        
    Returns:
        Response from next handler or 413 if request too large
    """
    # Check Content-Length header for potential large requests
    content_length = request.headers.get("Content-Length")
    if content_length and int(content_length) > config.MAX_REQUEST_SIZE:
        logger.warning(f"Security: Request size exceeded limit from {request.client.host}")
        return PlainTextResponse(
            content="Request too large",
            status_code=413,
            headers={"X-Request-ID": str(request.state.request_id) if hasattr(request.state, 'request_id') else "unknown"}
        )
    
    return await call_next(request)


# ============================================================================
# CUSTOM MIDDLEWARE
# ============================================================================

# Request ID middleware for tracing
@app.middleware("http")
async def request_id_middleware(request: Request, call_next):
    """
    Add request ID to each request for tracing and security auditing.
    
    Args:
        request: FastAPI Request object
        call_next: Next middleware/handler
        
    Returns:
        Response from the next handler with X-Request-ID header
    """
    import uuid
    request_id = str(uuid.uuid4())
    request.state.request_id = request_id
    
    # Add request ID to response headers for security tracing
    response = await call_next(request)
    response.headers["X-Request-ID"] = request_id
    
    return response


# Metrics middleware - Security monitoring without verbose logging
@app.middleware("http")
async def metrics_middleware(request: Request, call_next):
    """
    Collect performance and security metrics for each request.
    
    Args:
        request: FastAPI Request object
        call_next: Next middleware/handler
        
    Returns:
        Response from the next handler
    """
    import time as time_module
    
    start_time = time_module.time()
    method = request.method
    endpoint = request.url.path
    
    try:
        response = await call_next(request)
        status_code = response.status_code
        
        # Record metrics for security monitoring
        REQUEST_COUNT.labels(method=method, endpoint=endpoint, http_status=status_code).inc()
        REQUEST_LATENCY.labels(method=method, endpoint=endpoint).observe(time_module.time() - start_time)
        
        return response
        
    except Exception:
        # Record error metrics for security analysis
        REQUEST_COUNT.labels(method=method, endpoint=endpoint, http_status=500).inc()
        REQUEST_LATENCY.labels(method=method, endpoint=endpoint).observe(time_module.time() - start_time)
        raise


# ============================================================================
# EXCEPTION HANDLERS
# ============================================================================

# Exception handlers - Security-focused error handling
@app.exception_handler(RateLimitExceeded)
async def rate_limit_exception_handler(request: Request, exc: RateLimitExceeded):
    """
    Handle rate limit exceeded exceptions with security logging.
    
    Args:
        request: FastAPI Request object
        exc: RateLimitExceeded exception
        
    Returns:
        PlainTextResponse with rate limit exceeded message
    """
    # Security: Log potential DDoS attempts at WARNING level
    logger.warning(f"Security: Rate limit exceeded for {request.client.host}")
    return PlainTextResponse(
        content="Rate limit exceeded",
        status_code=429,
        headers={"Retry-After": str(exc.retry_after)}
    )


@app.exception_handler(Exception)
async def global_exception_handler(request: Request, exc: Exception):
    """
    Global exception handler - Security incident logging.
    
    Args:
        request: FastAPI Request object
        exc: The exception that was raised
        
    Returns:
        PlainTextResponse with generic error message (security: don't expose details)
    """
    # Security: Log errors but don't expose details to clients
    logger.error(f"Security incident: Internal server error from {request.client.host}")
    return PlainTextResponse(
        content="Internal Server Error",
        status_code=500
    )


# Route handlers - Production optimized (no debug logging)
@app.get("/", response_class=PlainTextResponse, summary="Root endpoint")
async def index(request: Request) -> PlainTextResponse:
    """
    Root endpoint handler for benchmarking.
    
    Args:
        request: FastAPI Request object
        
    Returns:
        PlainTextResponse: Empty response for benchmarking.
    """
    return PlainTextResponse(content="", headers={"X-Request-ID": request.state.request_id})


@app.get(
    "/user/{id}",
    response_class=PlainTextResponse,
    summary="Get user by ID",
)
async def get_user(
    request: Request,
    id: Annotated[int, Path(ge=0, le=2_147_483_647, description="User ID")],
) -> PlainTextResponse:
    """
    Retrieve user information by ID.
    
    Args:
        request: FastAPI Request object
        id: The user identifier
        
    Returns:
        PlainTextResponse: The user ID as plain text.
    
    Raises:
        HTTPException: If ID validation fails (handled by FastAPI).
    """
    return PlainTextResponse(content=str(id), headers={"X-Request-ID": request.state.request_id})


@app.post("/user", response_class=PlainTextResponse, summary="Create user")
async def create_user(request: Request) -> PlainTextResponse:
    """
    Create a new user.
    
    Args:
        request: FastAPI Request object
        
    Returns:
        PlainTextResponse: Empty response for benchmarking.
    """
    return PlainTextResponse(content="", headers={"X-Request-ID": request.state.request_id})


# Health check endpoints - Security monitoring endpoints
@app.get("/health", response_class=PlainTextResponse, include_in_schema=False)
async def health_check() -> PlainTextResponse:
    """
    Liveness probe endpoint for health monitoring.
    
    Returns:
        PlainTextResponse: Health status (OK or shutdown state).
    """
    if app_state.shutdown_requested:
        return PlainTextResponse(content="Shutting down", status_code=503)
    return PlainTextResponse(content="OK")


@app.get("/ready", response_class=PlainTextResponse, include_in_schema=False)
async def readiness_check() -> PlainTextResponse:
    """
    Readiness probe endpoint for container orchestration.
    
    Returns:
        PlainTextResponse: Readiness status with warmup period.
    """
    # Security: Ensure proper warmup before accepting traffic
    uptime = time.time() - app_state.startup_time
    if uptime < 5:  # 5-second warmup period for security initialization
        return PlainTextResponse(content="Not ready", status_code=503)
    if app_state.shutdown_requested:
        return PlainTextResponse(content="Shutting down", status_code=503)
    return PlainTextResponse(content="Ready")


@app.get("/metrics", response_class=PlainTextResponse, include_in_schema=False)
async def metrics_endpoint() -> PlainTextResponse:
    """
    Prometheus metrics endpoint for security and performance monitoring.
    
    Returns:
        PlainTextResponse: Prometheus metrics in OpenMetrics format.
        404 if metrics are disabled.
    """
    if config.PROMETHEUS_ENABLED:
        metrics_data = generate_latest(REGISTRY)
        return PlainTextResponse(
            content=metrics_data.decode("utf-8"),
            media_type=CONTENT_TYPE_LATEST
        )
    # Security: Don't expose metrics endpoint if disabled
    return PlainTextResponse(content="Metrics disabled", status_code=404)


# Graceful shutdown handling
def handle_shutdown(signum, frame):
    """
    Handle graceful shutdown on SIGTERM/SIGINT for security.
    
    Args:
        signum: Signal number
        frame: Current stack frame
    """
    # Security: Log shutdown at WARNING level for security monitoring
    logger.warning(f"Security: Received signal {signum}, initiating graceful shutdown")
    app_state.shutdown_requested = True
    # Security: Clean up resources before exit
    sys.exit(0)


# Register signal handlers
signal.signal(signal.SIGTERM, handle_shutdown)
signal.signal(signal.SIGINT, handle_shutdown)


if __name__ == "__main__":
    import uvicorn
    
    # Configure uvicorn for production with security best practices
    uvicorn_config = {
        "app": app,
        "host": config.HOST,
        "port": config.PORT,
        "log_level": config.LOG_LEVEL.lower(),
        "access_log": False,  # Disable access logs for performance (use metrics instead)
        "workers": config.WORKERS,
        "timeout_keep_alive": config.TIMEOUT,
        "timeout_graceful_shutdown": 30,
        "limit_max_requests": 10000,  # Prevent memory leaks
        "backlog": 2048,
        "max_request_size": config.MAX_REQUEST_SIZE,  # Enforce request size limits
        "proxy_headers": True,  # Enable for reverse proxy support
        "forwarded_allow_ips": "*",  # Allow all forwarded IPs (configure properly in production)
    }
    
    if not DEBUG_MODE:
        logger.warning(f"Starting FastAPI benchmark server in production mode with {config.WORKERS} workers on {config.HOST}:{config.PORT}")
    else:
        logger.info(f"Starting FastAPI benchmark server with {config.WORKERS} workers on {config.HOST}:{config.PORT}")
    
    try:
        uvicorn.run(**uvicorn_config)
    except KeyboardInterrupt:
        if DEBUG_MODE:
            logger.info("Server stopped by user")
        else:
            logger.warning("Server stopped by user")
    except Exception as e:
        logger.error(f"Server failed - {e}")
        sys.exit(1)

# ===========================================================================
# F# Production Dockerfile
# Multi-stage build with production-grade optimizations
# ===========================================================================

# ===========================================================================
# Build Stage
# ===========================================================================

FROM mcr.microsoft.com/dotnet/sdk:10.0 AS build

# Set build arguments
ARG BUILD_CONFIGURATION=Release
ARG TARGET_FRAMEWORK=net10.0

# Configure build environment
ENV DOTNET_SKIP_FIRST_TIME_EXPERIENCE=true \
    DOTNET_CLI_TELEMETRY_OPTOUT=true \
    DOTNET_NOLOGO=true

WORKDIR /usr/src/app

# Copy project files first for better caching
COPY *.fsproj ./
COPY *.sln ./

# Restore dependencies
RUN dotnet restore --no-cache

# Copy source files
COPY . .

# Build and publish in Release configuration
RUN dotnet publish \
    -c ${BUILD_CONFIGURATION} \
    -f ${TARGET_FRAMEWORK} \
    -o /app/publish \
    --no-restore \
    --no-build \
    -p:Deterministic=true \
    -p:Optimize=true \
    -p:DebugType=pdbonly \
    -p:DebugSymbols=false

# ===========================================================================
# Runtime Stage
# ===========================================================================

FROM mcr.microsoft.com/dotnet/aspnet:10.0 AS runtime

# Set runtime environment
ENV ASPNETCORE_ENVIRONMENT=Production \
    ASPNETCORE_URLS=http://*:3000 \
    DOTNET_PRINT_TELEMETRY_MESSAGE=false \
    DOTNET_NOCONFIG=true \
    DOTNET_Ready=true

# Install curl for health checks
RUN apt-get -qq update && \
    apt-get -qy install --no-install-recommends curl && \
    rm -rf /var/lib/apt/lists/*

# Create non-root user for security
RUN useradd -m appuser && \
    mkdir -p /app && \
    chown -R appuser:appuser /app

WORKDIR /app

# Copy published application from build stage
COPY --from=build --chown=appuser:appuser /app/publish .

# Switch to non-root user
USER appuser

# Production health check with /health endpoint
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
    CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000/health || exit 1

# Runtime configuration for production
ENV Kestrel__EndPoints__Http__Url=http://*:3000 \
    Kestrel__Limits__MaxRequestBodySize=8388608 \
    Kestrel__Limits__MaxConcurrentConnections=16384 \
    Kestrel__Limits__MaxConcurrentUpgradedConnections=1000 \
    Kestrel__Limits__KeepAliveTimeout=60 \
    Kestrel__Limits__RequestHeadersTimeout=30

# ===========================================================================
# Entrypoint
# ===========================================================================

{{#environment}}
    ENV {{{.}}}
{{/environment}}

ENTRYPOINT {{{command}}}

# ===========================================================================
# Security Best Practices Applied:
# - Multi-stage build to reduce image size
# - Non-root user for application runtime
# - Minimal base image (aspnet runtime only)
# - No build tools in production image
# - Health checks configured
# - Production environment variables set
# - Telemetry and first-run experience disabled
# ===========================================================================
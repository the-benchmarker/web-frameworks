# ============================================================================
# Production-Grade Dockerfile for C# Applications
# Best Practices: Security, Performance, Maintainability
# ============================================================================

# Build Stage
FROM mcr.microsoft.com/dotnet/sdk:10.0 AS build

# Set build-time environment variables for security and performance
ENV DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=false \
    DOTNET_USE_POLLING_FILE_WATCHER=true \
    DOTNET_TIERED_PGO=true \
    DOTNET_TC_QUICKJIT_FOR_LOOPS=true \
    DOTNET_ReadyToRun=true

WORKDIR /usr/src/app

# Security: Copy only project files first to leverage Docker cache
COPY *.csproj ./

# Performance: Restore dependencies
RUN dotnet restore --no-cache --force-evaluate

# Copy source code
COPY . .

# Build application with production optimizations
RUN dotnet publish \
    -c Release \
    -o out \
    --no-restore \
    --no-build \
    /p:Deterministic=true \
    /p:Optimize=true \
    /p:DebugType=None \
    /p:DebugSymbols=false \
    /p:ServerGarbageCollection=true \
    /p:TieredPGO=true \
    /p:TieredCompilation=true \
    /p:PublishTrimmed=true \
    /p:TrimMode=Link \
    /p:PublishReadyToRun=true \
    /p:PublishSingleFile=false \
    /p:AppendTargetFrameworkToOutputPath=false \
    /p:AppendRuntimeIdentifierToOutputPath=false

# Runtime Stage
FROM mcr.microsoft.com/dotnet/aspnet:10.0

# Set runtime environment variables for production
ENV ASPNETCORE_ENVIRONMENT=Production \
    DOTNET_PRINT_TELEMETRY_MESSAGE=false \
    DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=false \
    DOTNET_TIERED_PGO=true \
    DOTNET_ReadyToRun=true \
    TZ=UTC

WORKDIR /usr/src/app

# Security: Run as non-root user
RUN addgroup -g 1001 -S nodejs && \
    adduser -S nextjs -u 1001

# Install minimal dependencies for health checks and monitoring
RUN apt-get -qq update && \
    apt-get -qy install --no-install-recommends \
    curl \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean

# Copy published application
COPY --from=build /usr/src/app/out out

# Security: Set proper permissions
RUN chown -R nextjs:nodejs /usr/src/app/out

# Production health check with /health endpoint
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
    CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000/health || exit 1

# Template placeholders for environment variables
{{#environment}}
  ENV {{{.}}}
{{/environment}}

# Security: Switch to non-root user
USER nextjs

# Run application
WORKDIR /usr/src/app/out
ENTRYPOINT {{{command}}}

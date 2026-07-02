# Production-grade Dart VM Dockerfile
# Optimized for development and testing with production security practices

FROM dart:3.12-slim

# Configure environment for production
ENV DEBIAN_FRONTEND=noninteractive \
    TZ=UTC \
    DART_VM_OPTIONS=--no-enable-asserts

# Set the working directory
WORKDIR /app

# Install ca-certificates for HTTPS support
RUN apt-get update && apt-get install -y ca-certificates curl && rm -rf /var/lib/apt/lists/*

# Copy pubspec.yaml and get dependencies
COPY pubspec.yaml pubspec.yaml
RUN dart pub get --no-precompile

# Copy app source code
{{#files}}
    COPY '{{source}}' '{{target}}'
{{/files}}

# Create non-root user for security
RUN useradd -r -u 1000 -g 1000 -m -d /app -s /bin/false appuser

# Change ownership of app directory
RUN chown -R appuser:appuser /app

# Security: Drop all privileges
USER appuser

# Production health check with /health endpoint
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000/health || exit 1

# Run as non-root user
ENTRYPOINT ["dart", "run", "--release", "/app/server.dart"]

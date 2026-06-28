# Production-grade Dockerfile for Perl Benchmark Servers
# Security best practices, performance optimizations, and clean code

# Use official Perl slim image for minimal attack surface
FROM perl:5.43-slim

# Set maintainer label
LABEL maintainer="The Benchmarker Team"
LABEL version="1.0.0"
LABEL description="Production-grade Perl benchmark servers"

# Set environment variables for security and performance
ENV APP_NAME="Perl Benchmark Server"
ENV APP_VERSION="1.0.0"
ENV SERVER_HOST="0.0.0.0"
ENV SERVER_PORT="3000"
ENV DEBUG_MODE="0"
ENV LOG_LEVEL="error"
ENV PERL5OPT="-w -O2"

WORKDIR /usr/src/app

# Switch to root to install system dependencies
USER root

# Security: Install minimal dependencies with cleanup
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    build-essential \
    curl \
    ca-certificates \
    libssl-dev && \
  rm -rf /var/lib/apt/lists/* && \
  apt-get -qy clean && \
  rm -rf /tmp/* /var/tmp/*

# Security: Create non-root user for application
RUN useradd --create-home --shell /bin/bash appuser

# Security: Set proper permissions
RUN mkdir -p /usr/src/app/logs && \
    chown -R appuser:appuser /usr/src/app && \
    chmod -R 755 /usr/src/app

USER opam

{{#files}}
  COPY --chown=appuser:appuser '{{source}}' '{{target}}'
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

# Production: Install dependencies without tests and with quiet output
RUN cpanm --installdeps . --notest --quiet --with-feature=accelerate

# Security: Switch to non-root user for running application
USER appuser

# Performance: Set ulimit for production
RUN ulimit -n 65536

# Health check with security considerations
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD curl --fail --silent --show-error http://127.0.0.1:3000/health || exit 1

# Security: Run as non-root user with minimal privileges
# Note: We use exec to replace the shell process with the application
ENTRYPOINT ["sh", "-c", "exec su-exec appuser:appuser {{{command}}}"]

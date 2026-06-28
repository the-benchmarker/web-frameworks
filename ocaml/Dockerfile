# Production-grade Dockerfile for OCaml Dream Benchmark Server
# Security best practices, performance optimizations, and clean code

# Use official OCaml base image with security updates
FROM ocaml/opam:debian-13-ocaml-5.2

# Set maintainer label
LABEL maintainer="The Benchmarker Team"
LABEL version="1.0.0"
LABEL description="Production-grade Dream benchmark server"

# Set environment variables for security and performance
ENV APP_NAME="Dream Benchmark Server"
ENV APP_VERSION="1.0.0"
ENV SERVER_HOST="0.0.0.0"
ENV SERVER_PORT="3000"
ENV DEBUG_MODE="false"
ENV LOG_LEVEL="error"
ENV OCAMLRUNPARAM="a"

WORKDIR /usr/src/app

# Switch to root to install system dependencies
USER root

# Security: Install minimal dependencies with cleanup
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    pkg-config \
    libev-dev \
    libssl-dev \
    libgmp-dev \
    curl \
    ca-certificates && \
  rm -rf /var/lib/apt/lists/* && \
  apt-get -qy clean && \
  rm -rf /tmp/* /var/tmp/*

# Security: Create non-root user for application
RUN useradd --create-home --shell /bin/bash appuser

USER opam

{{#files}}
  COPY --chown=appuser:appuser '{{source}}' '{{target}}'
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

# Security: Switch to non-root user for running application
USER appuser

# Security: Set proper permissions
RUN chmod -R 755 /usr/src/app && \
    chown -R appuser:appuser /usr/src/app

# Performance: Set ulimit for production
RUN ulimit -n 65536

# Health check with security considerations
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD curl --fail --silent --show-error http://127.0.0.1:3000/health || exit 1

# Security: Run as non-root user with minimal privileges
ENTRYPOINT ["sh", "-c", "exec su-exec appuser:appuser {{{command}}}"]

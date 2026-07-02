# Production-grade Julia Dockerfile
# Optimized for security, performance, and minimal image size

{{#language.version}}
  FROM julia:{{{.}}}
{{/language.version}}
{{^language.version}}
  FROM julia:1.12
{{/language.version}}

WORKDIR /usr/src/app

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    curl \
    ca-certificates && \
  rm -rf /var/lib/apt/lists/* && \
  apt-get clean

COPY server.jl Project.toml deps.jl ./

RUN julia deps.jl

# Security: Create non-root user for production
RUN useradd -r -u 1000 -g 1000 -m -d /usr/src/app -s /bin/false appuser && \
    chown -R appuser:appuser /usr/src/app

# Security: Drop all privileges
USER appuser

# Production health check with /health endpoint
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000/health || exit 1

ENTRYPOINT {{{command}}}

# Production-grade Ruby Dockerfile
# Optimized for security, performance, and minimal image size

{{#language.version}}
  FROM ruby:{{{.}}}-slim AS build
{{/language.version}}
{{^language.version}}
  FROM ruby:4.0-slim AS build
{{/language.version}}

WORKDIR /usr/src/app

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    build-essential \
    libjemalloc2 \
    curl \
    ca-certificates \
    {{#build_deps.length}}
    {{#build_deps}}{{.}} {{/build_deps}} \
    {{/build_deps.length}} && \
  rm -rf /var/lib/apt/lists/* && \
  apt-get clean

ENV BUNDLE_PATH="/usr/local/bundle" \
  LD_PRELOAD=libjemalloc.so.2

{{#environment}}
  ENV {{{.}}}
{{/environment}}

# Copy Gemfile first for better caching
COPY Gemfile Gemfile
COPY Gemfile.lock Gemfile.lock

RUN --mount=type=cache,id=bundler-gems-{{language.version}},target=${BUNDLE_PATH}/cache \
  bundle install --jobs 4 --retry 3

# Copy the rest of the application
{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

# Runtime stage
FROM ruby:{{#language.version}}{{{.}}}{{/language.version}}{{^language.version}}4.0{{/language.version}}-slim

WORKDIR /usr/src/app

# Install minimal runtime dependencies
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    curl \
    ca-certificates \
    libjemalloc2 \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean

# Copy bundled gems from build stage
COPY --from=build /usr/local/bundle /usr/local/bundle

# Copy application code
{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

ENV BUNDLE_PATH="/usr/local/bundle" \
  LD_PRELOAD=libjemalloc.so.2

# Security: Create non-root user for production
RUN useradd -r -u 1000 -g 1000 -m -d /usr/src/app -s /bin/false appuser && \
    chown -R appuser:appuser /usr/src/app

# Security: Drop all privileges
USER appuser

# Health check with production settings
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}

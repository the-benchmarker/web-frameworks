{{#language.node.version}}
  FROM node:{{{.}}}-trixie-slim AS build
{{/language.node.version}}
{{^language.node.version}}
  FROM node:26.4-trixie-slim AS build
{{/language.node.version}}

WORKDIR /usr/src/app

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    curl \
    ca-certificates \
    {{#build_deps.length}}
    {{#build_deps}}{{.}} {{/build_deps}} \
    {{/build_deps.length}} && \
  rm -rf /var/lib/apt/lists/*

# Copy package files first for better caching
COPY package.json package.json
COPY package-lock.json package-lock.json

# Install dependencies with npm cache
RUN --mount=type=cache,id=npm-cache-{{language.node.version}},target=/root/.npm \
  npm ci --omit=dev --prefer-offline

# Copy the rest of the application
{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

# Runtime stage - use minimal node image
FROM node:{{#language.node.version}}{{{.}}}{{/language.node.version}}{{^language.node.version}}26.3{{/language.node.version}}-trixie-slim

WORKDIR /usr/src/app

# Install minimal runtime dependencies
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    curl \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean

# Copy built dependencies from build stage
COPY --from=build /usr/src/app/node_modules ./node_modules

# Copy application code
{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#fixes}}
  RUN {{{.}}}
{{/fixes}}

# Security: Create non-root user for production
RUN useradd -r -u 1000 -g 1000 -m -d /usr/src/app -s /bin/false appuser && \
    chown -R appuser:appuser /usr/src/app

# Security: Drop all privileges
USER appuser

# Production health check with /health endpoint
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000/health || exit 1

ENTRYPOINT {{{command}}}

# Production-grade Lua Dockerfile
# Optimized for security, performance, and minimal image size

FROM curlimages/curl AS curl

{{#image}}
  FROM {{{.}}}
{{/image}}
{{^image}}
  FROM alpine:latest
{{/image}}

WORKDIR /usr/src/app

{{#build_deps.length}}
  RUN apk add --no-cache {{#build_deps}}{{{.}}} {{/build_deps}}
{{/build_deps.length}}

{{#build}}
  RUN {{{.}}}
{{/build}}

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

COPY --from=curl /usr/bin/curl curl

# Security: Create non-root user for production
RUN addgroup -S appgroup && adduser -S appuser -G appgroup && \
    chown -R appuser:appgroup /usr/src/app

# Security: Drop all privileges
USER appuser:appgroup

# Production health check with /health endpoint
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000/health || exit 1

ENTRYPOINT {{{command}}}

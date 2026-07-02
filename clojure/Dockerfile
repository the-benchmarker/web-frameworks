{{#language.compiler.lein}}
  {{#language.java.version}}
    FROM clojure:temurin-{{{.}}}-lein-trixie-slim AS build
  {{/language.java.version}}
  {{^language.java.version}}
    FROM clojure:temurin-25-lein-trixie-slim AS build
  {{/language.java.version}}
{{/language.compiler.lein}}
{{^language.compiler.lein}}
  {{#language.java.version}}
    FROM clojure:temurin-{{{.}}}-tools-deps-trixie-slim AS build
  {{/language.java.version}}
  {{^language.java.version}}
    FROM clojure:temurin-25-tools-deps-trixie-slim AS build
  {{/language.java.version}}
{{/language.compiler.lein}}

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#build}}
  RUN {{{.}}}
{{/build}}
{{^build}}
  RUN clojure -Auberjar
{{/build}}



# Runtime stage
FROM eclipse-temurin:25.0.3_9-jre

# Install minimal runtime dependencies
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    curl \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/* && \
    apt-get clean

WORKDIR /opt/bin

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#binaries}}
  COPY --from=build /usr/src/app/{{{.}}} ./{{{.}}}
{{/binaries}}

# Security: Create non-root user for production
RUN useradd -r -u 1000 -g 1000 -m -d /opt/bin -s /bin/false appuser && \
    chown -R appuser:appuser /opt/bin

# Security: Drop all privileges
USER appuser

# Production health check with /health endpoint
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000/health || exit 1

ENTRYPOINT {{{command}}}

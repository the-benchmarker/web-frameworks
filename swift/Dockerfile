{{#language.version}}
  FROM swift:{{{.}}} AS build
{{/language.version}}
{{^language.version}}
  FROM swift:6.3 AS build
{{/language.version}}

WORKDIR /usr/src/app

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    libssl-dev \
    zlib1g-dev \
    libcurl4-openssl-dev \
    uuid-dev \
    ca-certificates && \
  rm -rf /var/lib/apt/lists/* && \
  apt-get clean

COPY Package.swift ./

RUN swift package resolve

COPY Sources/ ./Sources/

{{#build_environment}}
  ENV {{{.}}}
{{/build_environment}}

RUN swift build -c release -Xswiftc -enforce-exclusivity=unchecked



{{#language.version}}
  FROM swift:{{{.}}}-slim
{{/language.version}}
{{^language.version}}
  FROM swift:6.3-slim
{{/language.version}}

WORKDIR /app

# Install minimal runtime dependencies
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    curl \
    ca-certificates && \
  rm -rf /var/lib/apt/lists/* && \
  apt-get clean

{{#environment}}
  ENV {{{.}}}
{{/environment}}

COPY --from=build /usr/src/app/.build/release/server /app/.build/release/server

# Security: Create non-root user for production
RUN useradd -r -u 1000 -g 1000 -m -d /app -s /bin/false appuser && \
    chown -R appuser:appuser /app

# Security: Drop all privileges
USER appuser

# Health check with production settings
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}

{{#options}}
  CMD {{{options}}}
{{/options}}

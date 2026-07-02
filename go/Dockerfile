# Production-grade Go Dockerfile
# Optimized for security, performance, and minimal image size

{{#language.version}}
  FROM golang:{{{.}}} AS build
{{/language.version}}
{{^language.version}}
  FROM golang:1.26 AS build
{{/language.version}}

WORKDIR /go/src/app

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    build-essential \
    zlib1g-dev \
    ca-certificates && \
  rm -rf /var/lib/apt/lists/* && \
  apt-get clean

ENV CGO_ENABLED=1 \
  GOOS=linux \
  GOARCH={{#architecture}}{{{.}}}{{/architecture}}

{{#if.architecture.amd64}}
  # This compile with v3 microarchitecture, since used proc is zen2 compatible (AMD Ryzen 7 5700U)
  # See https://en.wikipedia.org/wiki/X86-64#Microarchitecture_levels
  ENV GOAMD64=v3
{{/if.architecture.amd64}}

{{#if.architecture.arm64}}
  # Use arm 8.5 instructions set
  # See https://en.wikipedia.org/wiki/Apple_M1
  ENV GOARM64=v8.5
{{/if.architecture.arm64}}

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

RUN --mount=type=cache,id=gomod-{{language.version}},target=/go/pkg/mod \
  go get

RUN --mount=type=cache,id=gomod-{{language.version}},target=/go/pkg/mod \
  --mount=type=cache,id=gobuild-{{language.version}},target=/root/.cache/go-build \
  go build -trimpath -ldflags="-w -s" {{#build_tags}} -tags {{{.}}} {{/build_tags}} -o /go/bin/app ./

# Runtime stage
FROM debian:trixie-slim

WORKDIR /go/bin

# Install minimal runtime dependencies
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    curl \
    ca-certificates \
    libstdc++6 \
    zlib1g && \
  rm -rf /var/lib/apt/lists/* && \
  apt-get clean

COPY --from=build /go/bin/app /go/bin/app

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#static_files}}
  COPY '{{source}}' '{{target}}'
{{/static_files}}

# Security: Create non-root user for production
RUN useradd -r -u 1000 -g 1000 -m -d /go/bin -s /bin/false appuser && \
    chown -R appuser:appuser /go/bin

# Security: Drop all privileges
USER appuser

# Health check with production settings
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}

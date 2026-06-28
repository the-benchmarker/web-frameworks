FROM debian:trixie-slim AS build

WORKDIR /usr/src/app

# Security best practices for Docker builds
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -y install --no-install-recommends \
    build-essential \
    wget \
    {{#build_deps}}{{{.}}} {{/build_deps}} && \
  rm -rf /var/lib/apt/lists/* && \
  apt-get clean

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#download}}
  RUN {{{.}}}
{{/download}}

{{#build}}
  RUN {{{.}}}
{{/build}}



FROM debian:trixie-slim

WORKDIR /opt/web

# Security hardening for production runtime
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    curl \
    {{#bin_deps}}{{{.}}} {{/bin_deps}} && \
  rm -rf /var/lib/apt/lists/* && \
  apt-get clean

{{#binaries}}
  COPY --from=build /usr/src/app/{{.}} {{{.}}}
{{/binaries}}

{{#static_files}}
  COPY '{{source}}' '{{target}}'
{{/static_files}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

# Security hardening for production containers
USER nobody:nobody

# Run as non-root user for security
RUN chown nobody:nobody /opt/web

# Health check for production monitoring
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD curl --fail http://0.0.0.0:3000 || exit 1

# Production entrypoint
ENTRYPOINT ["/opt/web/simple"]

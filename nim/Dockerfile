FROM nimlang/nim:2.2 AS build

WORKDIR /usr/src/app

{{#build_deps.length}}
  ARG DEBIAN_FRONTEND=noninteractive
  RUN apt-get -qq update && \
    apt-get -y install --no-install-recommends \
      ca-certificates \
      {{#build_deps}}{{{.}}} {{/build_deps}} && \
    rm -rf /var/lib/apt/lists/* && \
    apt-get clean
{{/build_deps.length}}

ENV NIM_ENV=production \
  NIMBLE_DIR=/home/nim/.nimble \
  PATH=$PATH:/home/nim/.nimble/bin:/root/.nimble/bin

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#build_with}}
  RUN {{{.}}}
{{/build_with}}
{{^build_with}}
  RUN nimble install -y
{{/build_with}}

{{#build_command}}
  RUN {{{.}}}
{{/build_command}}
{{^build_command}}
  RUN nim c {{#build_opts}} {{{.}}} {{/build_opts}} \
    --assertions:off \
    --warnings:off \
    --hints:off \
    -d:release \
    --opt:speed \
    --passC:-flto \
    --passL:-flto \
    server.nim
{{/build_command}}


FROM debian:trixie-slim

WORKDIR /usr/src/app

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    libssl-dev \
    libpcre2-dev \
    curl \
    ca-certificates \
    {{#runtime_deps}}{{{.}}} {{/runtime_deps}} && \
  rm -rf /var/lib/apt/lists/* && \
  apt-get clean

{{#custom_files}}
  COPY --from=build /usr/src/app/{{{.}}} /usr/src/app/{{{.}}}
{{/custom_files}}
{{^custom_files}}
  COPY --from=build /usr/src/app/server /usr/src/app/server
{{/custom_files}}

# Security: Create non-root user for production
RUN useradd -r -u 1000 -g 1000 -m -d /usr/src/app -s /bin/false appuser && \
    chown -R appuser:appuser /usr/src/app

# Security: Drop all privileges
USER appuser

# Health check with production settings
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}

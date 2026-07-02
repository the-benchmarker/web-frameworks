FROM debian:stable-slim AS build

WORKDIR /opt/vlang

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    build-essential \
    git \
    ca-certificates && \
  {{#build_deps.length}}
  apt-get -y install {{#build_deps}}{{.}} {{/build_deps}} && \
  {{/build_deps.length}}
  rm -rf /var/lib/apt/lists/* && \
  apt-get clean

ENV PATH=/opt/vlang:$PATH \
  GIT_SSL_NO_VERIFY=1


RUN git clone https://github.com/vlang/v /opt/vlang && \
    cd /opt/vlang && \
    git checkout {{language.version}} && \
    make

RUN ln -s /opt/vlang/v /usr/bin/v

{{#download}}
  RUN {{{.}}}
{{/download}}

WORKDIR /app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

RUN v {{#build_flags}} {{{.}}} {{/build_flags}} . -o server



FROM debian:stable-slim

WORKDIR /app

# Install minimal runtime dependencies
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -y install --no-install-recommends \
    curl \
    ca-certificates \
    liburing2 && \
  rm -rf /var/lib/apt/lists/* && \
  apt-get clean

# Copy the built binary
COPY --from=build /app /app

# Security: Create non-root user for production
RUN useradd -r -u 1000 -g 1000 -m -d /app -s /bin/false appuser && \
    chown -R appuser:appuser /app

# Security: Drop all privileges
USER appuser

# Production health check with /health endpoint
HEALTHCHECK --interval=30s --timeout=10s --start-period=40s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000/health || exit 1

ENTRYPOINT {{{command}}}

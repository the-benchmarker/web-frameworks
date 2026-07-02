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


# Pin V to the 0.5.1 release tag and build it from source, for a reproducible,
# auditable toolchain. Pinned by tag (not moving master) because post-0.5.1 master
# carries a codegen regression — single-element array push (`arr << x`) is 4-7x
# slower (vlang/v#27468), which slows every V program.
#
# Plain `make` can't build an old tag — its `latest_vc` step `git pull`s the newest
# vlang/vc bootstrap, which no longer matches 0.5.1's vlib ("unknown ident
# `native`"). So pin vc to the commit cut for 0.5.1 (vlang/vc f461dfeb =
# "[v:master] 0c3183c - V 0.5.1") and run make's own bootstrap recipe.
RUN git clone --branch 0.5.1 --depth 1 https://github.com/vlang/v /opt/vlang && \
  git clone --filter=blob:none --quiet https://github.com/vlang/vc /opt/vlang/vc && \
  git -C /opt/vlang/vc checkout -q f461dfebcdfa && \
  git clone --filter=blob:none --quiet --branch thirdparty-linux-amd64 \
    https://github.com/vlang/tccbin /opt/vlang/thirdparty/tcc && \
  cd /opt/vlang && \
  cc -std=c99 -w -o v1 vc/v.c -lm -lpthread && \
  ./v1 -no-parallel -o v2 cmd/v && \
  ./v2 -nocache -o v cmd/v && \
  rm -f v1 v2

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

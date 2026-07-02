{{#language.version}}
  FROM python:{{{.}}}-slim
{{/language.version}}
{{^language.version}}
  FROM python:3.14-slim
{{/language.version}}

WORKDIR /usr/src/app

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    curl \
    ca-certificates && \
  {{#build_deps.length}}
  apt-get -y install {{#build_deps}}{{{.}}} {{/build_deps}} && \
  {{/build_deps.length}}
  rm -rf /var/lib/apt/lists/* && \
  apt-get clean

{{#files}}
  COPY {{source}} {{target}}
{{/files}}

RUN --mount=type=cache,id=pip-cache-{{language.version}},target=/root/.cache/pip \
  pip install --no-cache-dir .

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

# Security: Create non-root user for production
RUN useradd -r -u 1000 -g 1000 -m -d /usr/src/app -s /bin/false appuser && \
    chown -R appuser:appuser /usr/src/app

# Security: Drop all privileges
USER appuser

# Health check with production settings
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}

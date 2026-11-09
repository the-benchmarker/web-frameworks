FROM debian:bookworm-slim

WORKDIR /usr/src/app

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends curl ca-certificates git libjavascriptcoregtk-4.1-0 libssl3 && \
  {{#build_deps.length}}
  apt-get -y install {{#build_deps}}{{.}} {{/build_deps}} && \
  {{/build_deps.length}}
  rm -rf /var/lib/apt/lists/*

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

# Carne runtime, pinned by language.engines.carne.version.
RUN ARCH=$(uname -m); \
  if [ "$ARCH" = "x86_64" ]; then CARNE_ARCH="x86_64"; else CARNE_ARCH="arm64"; fi; \
  curl -fsSL -o /tmp/carne.tar.gz https://github.com/openpeeps/carne/releases/download/{{language.engines.carne.version}}/carne_linux-${CARNE_ARCH}.tar.gz && \
  tar -xzf /tmp/carne.tar.gz -C /tmp && \
  mv /tmp/carne_linux-${CARNE_ARCH}/carne /usr/local/bin/carne && \
  rm -rf /tmp/carne.tar.gz /tmp/carne_linux-${CARNE_ARCH} && \
  carne --version

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#fixes}}
  RUN {{{.}}}
{{/fixes}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}

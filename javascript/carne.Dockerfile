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

# Carne runtime, pinned by language.carne.version.
RUN case "$(uname -m)" in \
      x86_64) CARNE_ARCH="x86_64" ;; \
      aarch64|arm64) CARNE_ARCH="arm64" ;; \
      *) echo "Unsupported architecture: $(uname -m)" >&2; exit 1 ;; \
    esac; \
    curl -fsSL -o /tmp/carne.tar.gz "https://github.com/openpeeps/carne/releases/download/{{language.carne.version}}/carne_linux-${CARNE_ARCH}.tar.gz" && \
    tar -xzf /tmp/carne.tar.gz -C /tmp && \
    mv "/tmp/carne_linux-${CARNE_ARCH}/carne" /usr/local/bin/carne && \
    rm -rf /tmp/carne.tar.gz "/tmp/carne_linux-${CARNE_ARCH}" && \
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

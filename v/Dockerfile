FROM debian:stable-slim as build

RUN apt -qq update && \
    apt -qy install --no-install-recommends build-essential git && \
    apt clean && \
    rm -rf /var/lib/apt/lists/*

ENV PATH=/opt/vlang:$PATH
ENV GIT_SSL_NO_VERIFY=1

WORKDIR /opt/vlang

RUN git clone --depth 1 https://github.com/vlang/v /opt/vlang && make

RUN ln -s /opt/vlang/v /usr/bin/v

{{#build_deps.length}}
  RUN apt -y update

  {{#build_deps}}
    RUN apt -y install {{{.}}}
  {{/build_deps}}

{{/build_deps.length}}

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

# Copy the built binary
COPY --from=build /app /app

RUN apt -y update && \
    apt -y install curl liburing2 && \
    apt clean && \
    rm -rf /var/lib/apt/lists/*

ENTRYPOINT {{command}}

HEALTHCHECK --interval=30s --timeout=10s --retries=3 --start-period=40s \
    CMD curl --fail http://0.0.0.0:3000 || exit 1

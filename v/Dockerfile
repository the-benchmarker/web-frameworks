FROM debian:stable-slim AS build

WORKDIR /opt/vlang

RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    build-essential \
    git && \
  {{#build_deps.length}}
  apt-get -y install {{#build_deps}}{{.}} {{/build_deps}} && \
  {{/build_deps.length}}
  rm -rf /var/lib/apt/lists/*

ENV PATH=/opt/vlang:$PATH \
  GIT_SSL_NO_VERIFY=1


RUN git clone https://github.com/vlang/v /opt/vlang && \
    cd /opt/vlang && \
    git checkout 7647ce1c6fad63b5578bc07883139906de74b2f8 && \
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

RUN apt-get -qq update && \
  apt-get -y install --no-install-recommends curl liburing2 && \
  rm -rf /var/lib/apt/lists/*

# Copy the built binary
COPY --from=build /app /app

HEALTHCHECK --interval=30s --timeout=10s --retries=3 --start-period=40s \
  CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}

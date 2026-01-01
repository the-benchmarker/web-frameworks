FROM debian:stable-slim as build

RUN apt -qq update
RUN apt -qy install --no-install-recommends build-essential git clang tcc \
 && apt clean \
 && rm -rf /var/lib/apt/lists/*

ENV PATH=/opt/vlang:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

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

COPY --from=build /app /app

ARG DEBIAN_FRONTEND=noninteractive
RUN apt -y update
RUN apt -qq update
RUN apt -qy install curl

ENTRYPOINT {{command}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

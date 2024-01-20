FROM debian as build

RUN apt-get -qq update
RUN apt-get -qy install build-essential git clang tcc

ENV PATH=/opt/vlang:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

WORKDIR /opt/vlang

RUN git clone --depth 1 https://github.com/vlang/v /opt/vlang && make

RUN ln -s /opt/vlang/v /usr/bin/v

WORKDIR /app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

RUN v . {{#build_flags}} {{{.}}} {{/build_flags}} -o server

FROM debian

{{#build_deps.length}}
  ARG DEBIAN_FRONTEND=noninteractive
  RUN apt-get -y update

  {{#build_deps}}
    RUN apt-get -y install {{{.}}}
  {{/build_deps}}

{{/build_deps.length}}

WORKDIR /app

COPY --from=build /app /app

ENTRYPOINT {{command}}

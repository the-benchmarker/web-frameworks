FROM swift:5.2

RUN apt -y update
RUN apt -y install libssl-dev zlib1g-dev libcurl4-openssl-dev uuid-dev

WORKDIR /usr/src/app

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

RUN swift build -c release

FROM swift:5.2-slim

WORKDIR /opt/web

{{#binaries}}
  COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/binaries}}

CMD {{#command}}{{{.}}}{{/command}}

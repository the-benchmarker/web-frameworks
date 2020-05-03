FROM ponylang/ponyc:0.33.2

RUN apt-get update
RUN apt-get install -y clang libssl-dev

WORKDIR /src/main

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

RUN stable fetch

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#before_command}}
  RUN {{{.}}}
{{/before_command}}

RUN stable env ponyc -Dopenssl_1.1.x

FROM ubuntu

WORKDIR /opt/web

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#files}}
  COPY {{{.}}} {{{.}}}
{{/files}}

{{#binaries}}
  COPY --from=0 /src/main/{{{.}}} {{{.}}}
{{/binaries}}

CMD ./main
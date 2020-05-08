FROM ponylang/ponyc:0.34.0

RUN apt-get update
RUN apt-get install -y clang libssl-dev

WORKDIR /opt/web

COPY bundle.json .

RUN stable fetch

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#before_command}}
  RUN {{{.}}}
{{/before_command}}

RUN stable env ponyc -Dopenssl_1.1.x -Dstatic

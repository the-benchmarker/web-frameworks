FROM ponylang/ponyc:0.35.1

RUN apt-get -y update && apt-get -y install clang libssl-dev

WORKDIR /src/main

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

FROM ubuntu 

RUN apt-get -y update && apt-get -y install openssl libatomic1

WORKDIR /opt

{{#binaries}}
  COPY --from=0 /src/main/{{{.}}} main
{{/binaries}}

CMD ./main

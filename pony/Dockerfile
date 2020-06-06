FROM ponylang/ponyc:0.35.1 as build

RUN apt-get update && apt-get -y install clang libssl-dev --no-install-recommends \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

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

# hadolint ignore=DL3006
FROM ubuntu 

RUN apt-get update && apt-get -y install openssl libatomic1 \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/web

{{#binaries}}
  COPY --from=build /src/main/main {{{.}}}
{{/binaries}}

CMD {{#command}}{{{.}}}{{/command}}

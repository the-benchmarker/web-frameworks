FROM ponylang/ponyc:0.35.1

RUN apt-get update
RUN apt-get install -y clang libssl-dev

WORKDIR /src/main

COPY bundle.json .
RUN stable fetch

COPY . ./

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#before_command}}
  RUN {{{.}}}
{{/before_command}}

RUN stable env ponyc -Dopenssl_1.1.x -Dstatic

FROM alpine

WORKDIR /opt

COPY --from=0 /src/main/main main

CMD ./main

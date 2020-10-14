FROM nimlang/nim:1.2.6-alpine

WORKDIR /usr/src/app

COPY . ./

{{#deps}}
  RUN apk --no-cache add {{{.}}}
{{/deps}}

ENV PATH $PATH:/root/.nimble/bin

RUN nimble install -y

RUN nim c {{#build_opts}} {{{.}}} {{/build_opts}} \
    --excessiveStackTrace:off \
    -d:release \
    --threads:on \
    server.nim

FROM alpine

COPY --from=0 /usr/src/app/server /usr/src/app/server

CMD /usr/src/app/server

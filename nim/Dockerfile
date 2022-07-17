FROM nimlang/nim:1.6.4-alpine

WORKDIR /usr/src/app

COPY . ./

{{#deps}}
  RUN apk --no-cache add {{{.}}}
{{/deps}}

{{#alpine-deps}}
  RUN apk --no-cache add {{{.}}}
{{/alpine-deps}}

ENV PATH $PATH:/root/.nimble/bin
{{#build-enviromnents}}
  ENV {{{.}}}
{{/build-enviromnents}}

RUN nimble install -y

RUN nim c {{#build_opts}} {{{.}}} {{/build_opts}} \
    --excessiveStackTrace:off \
    -d:release \
    --passC:-flto \
    --passL:-flto \
    server.nim

FROM alpine

{{#run-enviromnents}}
  ENV {{{.}}}
{{/run-enviromnents}}

COPY --from=0 /usr/src/app/server /usr/src/app/server

CMD /usr/src/app/server

FROM nimlang/nim:1.2.4-alpine

WORKDIR /usr/src/app

COPY . ./

{{#deps}}
  RUN apk --no-cache add {{{.}}}
{{/deps}}

ENV PATH $PATH:/root/.nimble/bin

RUN nimble install -y

RUN nim c {{#build_opts}} {{{.}}} {{/build_opts}} \
    # -d:danger \
    -d:noSignalHandler \
    --gc:markAndSweep \
    --listFullPaths:off \
    --excessiveStackTrace:off \
    --passL:-static \
    --passL:"-s" \
    --passC:"-fno-ident" \
    --passC:"-flto" \
    --passC:"-ffast-math" \
    --passC:"-fsingle-precision-constant" \
    -d:release \
    --threads:on \
    server.nim

FROM alpine

COPY --from=0 /usr/src/app/server /usr/src/app/server

CMD /usr/src/app/server

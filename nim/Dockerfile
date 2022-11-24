FROM nimlang/nim:1.6.10-alpine

RUN apk update && \
    apk upgrade --no-cache

{{#deps}}
  RUN apk --no-cache add {{{.}}}
{{/deps}}

WORKDIR /usr/src/app

COPY . ./

ENV PATH $PATH:/root/.nimble/bin

RUN nimble install -y

RUN nim c {{#build_opts}} {{{.}}} {{/build_opts}} \
    --excessiveStackTrace:off \
    -d:release \
    --passC:-flto \
    --passL:-flto \
    server.nim

FROM alpine

RUN apk --no-cache add \
      pcre-dev

COPY --from=0 /usr/src/app/server /usr/src/app/server

CMD /usr/src/app/server

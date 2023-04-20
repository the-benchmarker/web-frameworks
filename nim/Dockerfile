FROM nimlang/nim:1.6.12-alpine

RUN apk update && \
    apk upgrade --no-cache

{{#deps}}
  RUN apk --no-cache add {{{.}}}
{{/deps}}

WORKDIR /usr/src/app

COPY . ./

ENV PATH $PATH:/root/.nimble/bin

RUN nimble install -y

{{#build_command}}
  RUN {{{.}}}
{{/build_command}}
{{^build_command}}
  RUN nim c {{#build_opts}} {{{.}}} {{/build_opts}} \
    --excessiveStackTrace:off \
    -d:release \
    --passC:-flto \
    --passL:-flto \
    server.nim
{{/build_command}}

FROM alpine

WORKDIR /usr/src/app

RUN apk update && \
    apk upgrade --no-cache

RUN apk --no-cache add openssl-dev pcre-dev

{{#custom_files}}
  COPY --from=0 /usr/src/app/{{{.}}} /usr/src/app/{{{.}}}
{{/custom_files}}
{{^custom_files}}
  COPY --from=0 /usr/src/app/server /usr/src/app/server
{{/custom_files}}

{{#command}}
  CMD {{{.}}}
{{/command}}
{{^command}}
  CMD /usr/src/app/server
{{/command}}

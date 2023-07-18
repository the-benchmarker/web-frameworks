FROM alpine:3.18 AS build
RUN apk update && \
    apk upgrade --no-cache
RUN apk add --no-cache gcc ldc dub ldc-runtime musl-dev

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

ENV DC=ldc2
RUN dub build -b release --compiler=ldc2 --verbose

FROM alpine

RUN apk --no-cache add openssl-dev pcre-dev

{{#deps}}
  RUN apk --co-cache add {{{.}}}
{{/deps}}

COPY --from=build /usr/src/app/server /usr/src/app/server

{{#environment}}
  ENV {{{.}}}
{{/environment}}

CMD {{{command}}}

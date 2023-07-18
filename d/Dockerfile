FROM alpine AS build
RUN apk update && \
    apk upgrade --no-cache
RUN apk add --no-cache gcc ldc dub ldc-runtime musl-dev openssl-dev pcre-dev


WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

ENV DC=ldc2
RUN dub build -b release --compiler=ldc2 --verbose

{{#deps}}
  RUN apk --co-cache add {{{.}}}
{{/deps}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

CMD {{{command}}}

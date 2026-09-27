FROM curlimages/curl AS curl

FROM openresty/openresty:1.31-alpine-fat AS build

WORKDIR /usr/src/app

{{#build_deps.length}}
  RUN apk add --no-cache {{#build_deps}}{{{.}}} {{/build_deps}}
{{/build_deps.length}}

{{#build}}
  RUN {{{.}}}
{{/build}}

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

COPY --from=curl /usr/bin/curl curl
HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}

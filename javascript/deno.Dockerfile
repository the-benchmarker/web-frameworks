FROM denoland/deno:alpine-1.34.2

WORKDIR /usr/src/app

{{#deps}}
  RUN apk add {{{.}}}
{{/deps}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

CMD {{{command}}}
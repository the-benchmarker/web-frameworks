FROM denoland/deno:alpine-1.34.2

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

CMD {{{command}}}
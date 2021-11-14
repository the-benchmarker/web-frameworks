FROM denoland/deno:alpine-1.16.1

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

CMD {{{command}}}
FROM denoland/deno:latest

WORKDIR /usr/src/app

RUN apt-get -aa update

{{#deps}}
  RUN apt-get -qy install {{{.}}}
{{/deps}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

CMD {{{command}}}

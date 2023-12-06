FROM denoland/deno:1.38.5

WORKDIR /usr/src/app

RUN apt-get -qq update

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

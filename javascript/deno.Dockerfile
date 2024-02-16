FROM denoland/deno:latest

WORKDIR /usr/src/app

{{#deps.length}}
  ARG DEBIAN_FRONTEND=noninteractive
  RUN apt-get -qq update

  {{#deps}}
    RUN apt-get -qy install {{{.}}}
  {{/deps}}

{{/deps.length}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

CMD {{{command}}}

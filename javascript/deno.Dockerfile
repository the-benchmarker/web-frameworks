FROM denoland/deno:2.0.6

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

RUN apt-get -qq update
RUN apt-get -qy install curl
HEALTHCHECK --interval=1m --timeout=60s --retries=3 CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}

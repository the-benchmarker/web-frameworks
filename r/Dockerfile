FROM r-base:4.5.2

WORKDIR /usr/src/app

{{#build_deps.length}}
  ARG DEBIAN_FRONTEND=noninteractive
  RUN apt-get -y update

  {{#build_deps}}
    RUN apt-get -y install {{{.}}}
  {{/build_deps}}

{{/build_deps.length}}

RUN R  -e 'install.packages("remotes")'

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN R -e 'remotes::install_deps()'
RUN apt-get -qq update
#RUN apt-get install curl=8.16.0-4 libcurl4t64=8.16.0-4

ENTRYPOINT {{{command}}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

FROM r-base:4.5.1
WORKDIR /usr/src/app

RUN apt-get -qq update

{{#deps}}
  RUN apt-get -y install {{{.}}}
{{/deps}}

RUN R  -e 'install.packages("remotes")'

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN R -e 'remotes::install_deps()'
RUN apt-get -qq update
RUN apt-get -qy install curl

ENTRYPOINT {{{command}}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

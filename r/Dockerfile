FROM r-base:4.4.1
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

CMD {{{command}}}


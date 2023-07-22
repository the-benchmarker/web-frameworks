FROM rocker/r-ver:4.3.1
WORKDIR /usr/src/app

RUN R  -e 'install.packages("remotes")'

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN R -e 'remotes::install_deps()'

CMD {{{command}}}


FROM rocker/r-ver:4.2.2
WORKDIR /usr/src/app

RUN R  -e 'install.packages("remotes")'

COPY . ./

{{#bootstrap}}
  RUN R -e "{{{.}}}"
{{/bootstrap}}
{{^bootstrap}}
  RUN R -e 'remotes::install_deps()'
{{/bootstrap}}

CMD R -e 'source("app.R")'


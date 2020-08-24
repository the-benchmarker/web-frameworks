FROM rocker/r-ver:4.0.2
WORKDIR /usr/src/app

{{#before_command}}
  RUN {{{.}}}
{{/before_command}}

COPY . ./

CMD R -e 'source("app.R")'


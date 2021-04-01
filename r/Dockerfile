FROM rocker/r-ver:4.0.4
WORKDIR /usr/src/app

RUN R  -e 'install.packages("remotes")'

COPY . ./

RUN R -e 'remotes::install_deps()'

CMD R -e 'source("app.R")'


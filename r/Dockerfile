FROM rocker/r-ver:4.2.3
WORKDIR /usr/src/app

RUN R  -e 'install.packages("remotes")'

COPY . ./

RUN R -e 'remotes::install_deps()'

CMD {{command}}


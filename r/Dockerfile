FROM r-base:4.5.3

WORKDIR /usr/src/app

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    apt-utils \
    ca-certificates \
    curl \
    libcurl4t64=8.18.0-2 --allow-downgrades && \
  {{#build_deps.length}}
  apt-get -qy install {{#build_deps}}{{{.}}} {{/build_deps}} && \
  {{/build_deps.length}}
  rm -rf /var/lib/apt/lists/*

RUN R  -e 'install.packages("remotes")'

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN R -e 'remotes::install_deps()'

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}

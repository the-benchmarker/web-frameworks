FROM r-base:4.5.2

WORKDIR /usr/src/app

{{#build_deps.length}}
  ARG DEBIAN_FRONTEND=noninteractive
  RUN apt-get -qq update && \
  {{#build_deps}}
      apt-get -qy install --no-install-recommends {{{.}}} && \
  {{/build_deps}}
      apt-get clean && rm -rf /var/lib/apt/lists/*

{{/build_deps.length}}

RUN R  -e 'install.packages("remotes")'

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN R -e 'remotes::install_deps()'

ENTRYPOINT {{{command}}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

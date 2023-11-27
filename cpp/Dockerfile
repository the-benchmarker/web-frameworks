FROM debian:bullseye

WORKDIR /usr/src/app

RUN echo "deb http://ftp.debian.org/debian bullseye-backports main" >> /etc/apt/sources.list
RUN apt-get -qq update
RUN apt-get remove --auto-remove cmake 
RUN apt-get -qy install build-essential git
RUN apt-get -qy install cmake/bullseye-backports cmake-data/bullseye-backports

{{#build_deps}}
  RUN apt-get -qy install {{{.}}}
{{/build_deps}}

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}
{{#download}}
  RUN {{{.}}}
{{/download}}

{{#build}}
   RUN {{{.}}}
{{/build}}

FROM debian:bookworm

{{#environment}}
ENV {{{.}}}
{{/environment}}

WORKDIR /opt/web

RUN apt-get -qq update

{{#bin_deps}}
   RUN apt-get -qy install {{{.}}}
{{/bin_deps}}

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}
{{#binaries}}
  COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/binaries}}

CMD {{{command}}}

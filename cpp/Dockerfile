FROM debian:bullseye

WORKDIR /usr/src/app

RUN apt-get -qq update
RUN apt-get -qy install build-essential git cmake

#RUN echo "deb http://ftp.debian.org/debian unstable main" >> /etc/apt/sources.list
#RUN apt-get -qq update
#RUN apt-get remove --auto-remove cmake 
#RUN apt-get -qy install cmake/unstable cmake-data/unstable

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

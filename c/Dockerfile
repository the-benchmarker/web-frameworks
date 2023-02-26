FROM debian:bullseye

WORKDIR /usr/src/app

RUN apt-get -qq update
RUN apt-get -qy install build-essential wget

{{#build_deps}}
  RUN apt-get -qy install {{{.}}}
{{/build_deps}}

WORKDIR /usr/src/app

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

{{#download}}
  RUN {{{.}}}
{{/download}}

{{#build}}
   RUN {{{.}}}
{{/build}}

FROM debian:bullseye

WORKDIR /opt/web

RUN apt-get -qq update

{{#bin_deps}}
   RUN apt-get -qy install {{{.}}}
{{/bin_deps}}

{{#sources}}
   COPY {{{.}}} {{{.}}}
{{/sources}}

{{#binaries}}
   COPY --from=0 /usr/src/app/{{.}} {{{.}}}
{{/binaries}}

CMD {{{command}}}

FROM fedora:32

RUN dnf -y install cmake jsoncpp-devel openssl-devel zlib-devel @development-tools g++ uuid-devel

WORKDIR /opt/web

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

{{#download}}
  RUN {{{.}}}
{{/download}}

{{#build}}
   RUN {{{.}}}
{{/build}}

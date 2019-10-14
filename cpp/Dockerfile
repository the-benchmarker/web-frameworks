FROM gcc:9.2

ENV DEBIAN_FRONTEND=noninteractive

WORKDIR /usr/src/app

RUN apt-get -qq update && \
    apt-get -qy install cmake {{#deps}} {{{.}}} {{/deps}}

COPY . ./

{{#clone}}
  RUN {{{.}}}
{{/clone}}

{{#build}}
   RUN {{{.}}}
{{/build}}

FROM debian

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update
{{#bin_deps}}
   RUN apt-get -qy install {{{.}}}
{{/bin_deps}}

{{#files}}
COPY --from=0 /usr/src/app/{{.}} /usr/bin/{{{.}}}
{{/files}}

{{#command}}
  CMD {{{.}}}
{{/command}}

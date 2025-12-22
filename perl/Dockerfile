FROM perl:5.43-slim

ENV DEBIAN_FRONTEND noninteractive
RUN apt-get -qq update && \
    apt-get -qy install --no-install-recommends build-essential curl && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#bootstrap}}
   RUN {{{.}}}
{{/bootstrap}}

RUN cpanm --installdeps . --with-feature=accelerate

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{command}}

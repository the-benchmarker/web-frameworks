FROM ubuntu:24.04 as build

RUN apt-get -qq update && \
    apt-get -qy install --no-install-recommends curl zlib1g-dev libssl-dev clang libxml2 xz-utils tzdata && \
    ln -fs /usr/share/zoneinfo/Europe/Rome /etc/localtime && \
    dpkg-reconfigure -f noninteractive tzdata && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

RUN curl -L -o ldc2.tar.xz https://github.com/ldc-developers/ldc/releases/download/v1.41.0/ldc2-1.41.0-linux-{{#arch}}{{{.}}}{{/arch}}.tar.xz && \
    tar xf ldc2.tar.xz && \
    rm ldc2.tar.xz && mv ldc2* ldc2

ENV PATH="$PATH:$CWD/ldc2/bin"

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

ENV DC=ldc2
RUN dub build -b release --compiler=ldc2

FROM ubuntu:24.04

WORKDIR /opt/web

RUN apt-get -qq update && \
    apt-get -qy install --no-install-recommends curl && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

COPY --from=build /usr/src/app/server /opt/web/server

USER nobody

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}

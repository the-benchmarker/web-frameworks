FROM ubuntu:24.04 as build

RUN apt-get update && apt-get upgrade -y

RUN apt-get install -yq curl zlib1g-dev libssl-dev clang libxml2 xz-utils tzdata && \
    ln -fs /usr/share/zoneinfo/Europe/Rome /etc/localtime && \
    dpkg-reconfigure -f noninteractive tzdata

RUN curl -L -o ldc2.tar.xz https://github.com/ldc-developers/ldc/releases/download/v1.40.0/ldc2-1.40.0-linux-{{#arch}}{{{.}}}{{/arch}}.tar.xz
RUN tar xf ldc2.tar.xz
RUN rm ldc2.tar.xz && mv ldc2* ldc2
ENV PATH="$PATH:$CWD/ldc2/bin"

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

ENV DC=ldc2
RUN dub build -b release --compiler=ldc2

FROM ubuntu:24.04

WORKDIR /opt/web

RUN apt-get -qq update
RUN apt-get -qy install curl

COPY --from=build /usr/src/app/server /opt/web/server

USER nobody

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}

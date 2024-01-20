FROM debian as build

RUN apt-get -qq update
RUN apt-get -qy install build-essential

ENV PATH=/opt/vlang:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

WORKDIR /opt/vlang

RUN git clone --depth 1 https://github.com/vlang/v /opt/vlang && make

RUN ln -s /opt/vlang/v /usr/bin/v

WORKDIR /app

RUN v up

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

RUN v . {{#build_flags}} {{{.}}} {{/build_flags}} -o server

FROM debian

WORKDIR /app

COPY --from=build /app /app

CMD {{command}}

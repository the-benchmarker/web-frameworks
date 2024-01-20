FROM debian as build

RUN apt-get -qq update
RUN apt-get -qy install build-essential

WORKDIR /opt/vlang

ENV PATH=/opt/vlang:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

CMD ["bash"]

RUN /bin/sh -c mkdir -p /opt/vlang && ln -s /opt/vlang/v /usr/bin/v # buildkit

WORKDIR /opt/vlang

RUN /bin/sh -c git clone --depth 1 https://github.com/vlang/v /opt/vlang && make

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

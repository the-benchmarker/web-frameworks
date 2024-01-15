FROM thevlang/vlang:debian-dev as build

WORKDIR /app

RUN apt-get -qq update
RUN apt-get -qy install build-essential

RUN v up

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

RUN v . {{#build_flags}} {{{.}}} {{/build_flags}} -o server

FROM thevlang/vlang:debian-dev

WORKDIR /app

COPY --from=build /app /app

CMD {{command}}

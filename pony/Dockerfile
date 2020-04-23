FROM ponylang/ponyc:0.33.2

RUN apt-get update
RUN apt-get install -y clang libssl-dev

WORKDIR /src/main

COPY bundle.json .
RUN stable fetch

COPY . ./

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#before_command}}
  RUN {{{.}}}
{{/before_command}}

RUN stable env ponyc -Dopenssl_1.1.x

{{#command}}
  CMD {{{.}}}
{{/command}}

{{^command}}
  CMD ./main
{{/command}}


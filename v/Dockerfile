FROM taojy123/vlang:v0.1.24

WORKDIR /usr/src/app

{{#before_command}}
  RUN {{{.}}}
{{/before_command}}

COPY . ./

RUN v build main.v

FROM ubuntu

WORKDIR /opt/web

COPY --from=0 /usr/src/app/main main

CMD ./main

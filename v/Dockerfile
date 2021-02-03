FROM gladmo/vlang:0.2.2-alpine

WORKDIR /usr/src/app

{{#before_command}}
  RUN {{{.}}}
{{/before_command}}

COPY . ./

RUN v main.v

# hadolint ignore=DL3006
FROM alpine

WORKDIR /opt/web

COPY --from=0 /usr/src/app/main main

CMD ./main

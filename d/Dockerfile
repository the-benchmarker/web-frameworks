FROM debian:bookworm AS build

WORKDIR /usr/src/app

RUN apt-get -qq update
RUN apt-get -qy install build-essential wget openssl ldc dub libssl-dev zlib1g-dev

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

ENV DC=ldc2
RUN dub build -b release --compiler=ldc2 --verbose

FROM debian:bookworm

WORKDIR /opt/web

RUN apt-get -qq update && apt-get -qy install openssl wget ldc nginx

COPY --from=build /usr/src/app/server /opt/web/server

{{#deps}}
  RUN apt-get -qy install {{{.}}}
{{/deps}}

{{#static_files}}
  COPY '{{source}}' '{{target}}'
{{/static_files}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

RUN echo 'server {\n\
    listen 0.0.0.0:3000;\n\
    location / {\n\
       proxy_pass http://127.0.0.1:8080;\n\
    }\n\
}\n' > /etc/nginx/conf.d/default.conf
  
CMD  nohup {{{command}}} ; /usr/sbin/nginx

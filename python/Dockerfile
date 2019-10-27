FROM python:3.8-alpine

RUN apk add build-base python3-dev

WORKDIR /usr/src/app

{{#build_deps}}
  RUN apk add {{{.}}}
{{/build_deps}}

COPY . ./

{{#fixes}}
  RUN {{{.}}}
{{/fixes}}

RUN pip3 install -r requirements.txt

CMD {{command}}

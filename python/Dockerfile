FROM python:3.8-alpine

RUN apk add build-base

WORKDIR /usr/src/app

{{#build_deps}}
  RUN apk add {{{.}}}
{{/build_deps}}

COPY . ./

{{#fixes}}
  RUN {{{.}}}
{{/fixes}}

RUN pip install -r requirements.txt

CMD {{command}}

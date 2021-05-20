FROM python:3.9-slim

WORKDIR /usr/src/app

RUN apt-get -qq update
RUN apt-get -ay install build-essentials

{{#build_deps}}
  RUN apt-get add {{{.}}}
{{/build_deps}}

COPY . ./

{{#fixes}}
  RUN {{{.}}}
{{/fixes}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

RUN pip install -r requirements.txt

CMD {{command}}

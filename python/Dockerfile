FROM python:3.10-slim

WORKDIR /usr/src/app

RUN apt-get -qq update
RUN apt-get -qy install build-essential

{{#build_deps}}
  RUN apt-get  -qy install {{{.}}}
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

FROM perl:5.30-slim

RUN apt-get -qq update && \
  apt-get -qy install build-essential

WORKDIR /usr/src/app
COPY cpanfile d2app.psgi ./
RUN cpanm --notest --installdeps .

EXPOSE 3000
CMD {{#command}}{{{.}}}{{/command}}

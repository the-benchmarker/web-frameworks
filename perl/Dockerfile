FROM perl:5.39-slim

RUN apt-get -qq update && \
  apt-get -qy install build-essential

WORKDIR /usr/src/app
COPY cpanfile d2app.psgi ./
RUN cpanm --notest --installdeps .

ENTRYPOINT {{command}}

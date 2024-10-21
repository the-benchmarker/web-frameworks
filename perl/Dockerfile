FROM perl:5.40-slim

RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends build-essential

WORKDIR /usr/src/app
COPY cpanfile d2app.psgi ./
RUN cpanm --notest --installdeps .

CMD {{command}}

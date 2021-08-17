FROM perl:5.34-slim

RUN apt-get -qq update && \
  apt-get -qy install build-essential

WORKDIR /usr/src/app
COPY cpanfile d2app.psgi ./
RUN cpanm --notest --installdeps .

EXPOSE 8080
CMD plackup -s Gazelle --port 8080 --env production --app d2app.psgi

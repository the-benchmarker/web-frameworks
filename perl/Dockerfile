FROM perl:5.32-slim

RUN apt-get update && apt-get install -y build-essential  --no-install-recommends \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /usr/src/app
COPY cpanfile d2app.psgi ./
RUN cpanm --notest --installdeps .

EXPOSE 3000
CMD {{#command}}{{{.}}}{{/command}}

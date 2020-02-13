FROM google/dart

WORKDIR /app

ADD pubspec.* /app/

RUN pub get --no-precompile

ADD . /app/

RUN pub get --offline --no-precompile

WORKDIR /app

EXPOSE 8888

ENTRYPOINT ["dart", "bin/main.dart"]
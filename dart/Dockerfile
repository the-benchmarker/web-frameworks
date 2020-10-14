FROM google/dart:2.10

WORKDIR /app

ADD pubspec.* /app/

RUN pub get --no-precompile

ADD . /app/

RUN pub get --offline --no-precompile

WORKDIR /app

CMD dart main.dart

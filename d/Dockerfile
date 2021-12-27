FROM dlang2/dmd-ubuntu:2.096.1 AS build

WORKDIR /usr/src/app

COPY . ./

RUN dub build -b release

FROM debian:bullseye-slim

COPY --from=build /usr/src/app/server /usr/src/app/server

CMD /usr/src/app/server

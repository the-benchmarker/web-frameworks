FROM swift:5.2

RUN apt -y update
RUN apt -y install libssl-dev zlib1g-dev libcurl4-openssl-dev uuid-dev

WORKDIR /usr/src/app

COPY Package.swift ./
COPY Sources Sources

RUN swift package resolve

RUN swift build \
        -c release \
        -Xswiftc -g

FROM swift:5.2-slim

WORKDIR /usr/src/app

COPY --from=0 /usr/src/app/.build/release/Run .

CMD ./Run {{options}}

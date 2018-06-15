FROM haskell

WORKDIR /usr/src/app
COPY . .

RUN cabal update
RUN cabal new-configure -O2
RUN cabal new-build

EXPOSE 3000
CMD cabal new-run

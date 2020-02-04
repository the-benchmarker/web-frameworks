FROM elixir:1.10-alpine

RUN mix local.rebar --force
RUN mix local.hex --force

WORKDIR /usr/src/app

COPY . ./

RUN mix deps.get

ENV MIX_ENV prod

CMD {{command}}

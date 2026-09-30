FROM mcr.microsoft.com/dotnet/sdk:10.0 AS build

WORKDIR /usr/src/app

COPY *.fsproj .

RUN dotnet restore

COPY . .

RUN dotnet publish -c release -o out



FROM mcr.microsoft.com/dotnet/sdk:10.0

WORKDIR /usr/src/app

RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends curl && \
  rm -rf /var/lib/apt/lists/*

COPY --from=build /usr/src/app/out out

{{#environment}}
    ENV {{{.}}}
{{/environment}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}

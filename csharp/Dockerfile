FROM mcr.microsoft.com/dotnet/sdk:9.0 as build

WORKDIR /usr/src/app

# copy csproj and restore as distinct layers
COPY *.csproj .
RUN dotnet restore

# copy everything else and build app
COPY . .
RUN dotnet publish -c release -o out

FROM mcr.microsoft.com/dotnet/aspnet:9.0

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/out out

{{#environment}}
  ENV {{{.}}}
{{/environment}}

RUN apt-get -qq update
RUN apt-get -qy install curl
HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1 

ENTRYPOINT {{command}}

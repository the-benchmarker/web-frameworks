FROM mcr.microsoft.com/dotnet/core/sdk:3.1-alpine

WORKDIR /usr/src/app

# copy csproj and restore as distinct layers
COPY *.csproj .
RUN dotnet restore

# copy everything else and build app
{{#sources}}
    COPY {{{.}}} {{{.}}}
{{/sources}}

RUN dotnet publish -c release -o out

FROM mcr.microsoft.com/dotnet/core/sdk:3.1-alpine

WORKDIR /usr/src/app

COPY --from=0 /usr/src/app/out out

ENV ASPNETCORE_URLS http://*:3000
ENV COMPlus_TieredCompilation 1

CMD {{#command}}{{{.}}}{{/command}}


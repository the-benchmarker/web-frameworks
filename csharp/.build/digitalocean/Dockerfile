FROM mcr.microsoft.com/dotnet/core/sdk:3.1-alpine

WORKDIR /opt/web

# copy csproj and restore as distinct layers
COPY *.csproj .
RUN dotnet restore

# copy everything else and build app
{{#sources}}
    COPY {{{.}}} {{{.}}}
{{/sources}}

RUN dotnet publish -c release -o out
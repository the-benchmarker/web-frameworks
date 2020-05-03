FROM mcr.microsoft.com/dotnet/core/sdk:3.1-alpine as build

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

COPY --from=build /usr/src/app/out out

ENV COMPlus_TieredCompilation 1

CMD dotnet /usr/src/app/out/web.dll

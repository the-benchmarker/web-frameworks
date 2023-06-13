FROM mcr.microsoft.com/dotnet/sdk:7.0-alpine AS build

WORKDIR /usr/src/app

# copy fsproj and restore as distinct layers
COPY *.fsproj .
RUN dotnet restore

# copy everything else and build app
COPY . .
RUN dotnet publish -c release -o out

FROM mcr.microsoft.com/dotnet/aspnet:7.0-alpine

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/out out

{{#environment}}
    ENV {{{.}}}
{{/environment}}


CMD {{command}}

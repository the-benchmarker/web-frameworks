FROM mcr.microsoft.com/dotnet/sdk:8.0 as build

WORKDIR /usr/src/app

# copy csproj and restore as distinct layers
COPY *.csproj .
RUN dotnet restore

# copy everything else and build app
COPY . .
RUN dotnet publish -c release -o out

FROM mcr.microsoft.com/dotnet/aspnet:8.0

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/out out

{{#environment}}
  ENV {{{.}}}
{{/environment}}

ENTRYPOINT {{command}}

FROM mcr.microsoft.com/dotnet/core/sdk:3.0-alpine

WORKDIR /usr/src/app

# copy csproj and restore as distinct layers
COPY *.csproj .
RUN dotnet restore

# copy everything else and build app
COPY . .
RUN dotnet publish -c release -o out

FROM mcr.microsoft.com/dotnet/core/aspnet:3.0-alpine

WORKDIR /usr/src/app

{{#files}}
COPY --from=0 /usr/src/app/out/{{{.}}} {{{.}}}
{{/files}}

ENV ASPNETCORE_URLS http://*:3000
ENV COMPlus_TieredCompilation 1

CMD dotnet aspnetcore.dll

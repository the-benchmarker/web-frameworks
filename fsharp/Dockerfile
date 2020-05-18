FROM fsharp:10.7-netcore AS build

WORKDIR /usr/src/app

# copy fsproj and restore as distinct layers
COPY *.fsproj .
RUN dotnet restore

# copy everything else and build app
{{#sources}}
    COPY {{{.}}} {{{.}}}
{{/sources}}


RUN dotnet publish -c release -o out

FROM fsharp:10.7-netcore

WORKDIR /opt/web

COPY --from=build /usr/src/app/out out

ENV ASPNETCORE_URLS=http://+:3000
ENV COMPlus_TieredCompilation 1
EXPOSE 3000

CMD {{#command}}{{{.}}}{{/command}}

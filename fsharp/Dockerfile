FROM fsharp:10.7-netcore

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

COPY --from=0 /usr/src/app/out out

ENV COMPlus_TieredCompilation 1

CMD {{#command}}{{{.}}}{{/command}}

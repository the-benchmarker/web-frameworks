FROM fsharp:10.7-netcore

WORKDIR /opt/web

# copy fsproj and restore as distinct layers
COPY *.fsproj .
RUN dotnet restore

# copy everything else and build app
{{#sources}}
    COPY {{{.}}} {{{.}}}
{{/sources}}


RUN dotnet publish -c release -o out
FROM mcr.microsoft.com/dotnet/core/sdk:3.1-alpine

WORKDIR /usr/src/app

# copy fsproj and restore as distinct layers
COPY *.fsproj .
RUN dotnet restore

# copy everything else and build app
COPY . .
RUN dotnet publish -c release -o out

FROM mcr.microsoft.com/dotnet/core/aspnet:3.1-alpine

WORKDIR /usr/src/app

COPY --from=0 /usr/src/app/out out

ENV COMPlus_TieredCompilation 1

CMD dotnet /usr/src/app/out/suaveapp.dll

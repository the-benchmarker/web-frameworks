FROM mcr.microsoft.com/dotnet/sdk:6.0-alpine AS build

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

ENV ASPNETCORE_URLS=http://+:3000
ENV COMPlus_TieredCompilation 1
EXPOSE 3000

CMD dotnet /usr/src/app/out/web.dll

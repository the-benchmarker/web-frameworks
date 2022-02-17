FROM mcr.microsoft.com/dotnet/sdk:6.0 as build

WORKDIR /usr/src/app

# copy csproj and restore as distinct layers
COPY *.csproj .
RUN dotnet restore

# copy everything else and build app
COPY . .
RUN dotnet publish -c release -o out

FROM mcr.microsoft.com/dotnet/aspnet:7.0

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/out out

ENV ASPNETCORE_URLS http://*:3000
ENV DOTNET_TieredPGO 1 
ENV DOTNET_TC_QuickJitForLoops 1 
ENV DOTNET_ReadyToRun 0

CMD dotnet /usr/src/app/out/web.dll

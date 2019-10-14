FROM microsoft/dotnet:2.2-sdk AS build
WORKDIR /app

# copy fsproj and restore as distinct layers
COPY *.fsproj .
RUN dotnet restore

# copy everything else and build app
COPY . .
RUN dotnet publish -c release -o out

FROM microsoft/dotnet:2.2-aspnetcore-runtime AS runtime
WORKDIR /app
COPY --from=build /app/out ./

ENV COMPlus_TieredCompilation 1

ENTRYPOINT ["dotnet", "suaveapp.dll"]

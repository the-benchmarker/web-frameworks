FROM microsoft/aspnetcore

# install sdk
# @see https://www.microsoft.com/net/learn/get-started/linux/debian9
RUN apt-get -qq update
RUN apt-get -qy install curl libunwind8 gettext apt-transport-https gnupg
RUN curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > microsoft.gpg
RUN mv microsoft.gpg /etc/apt/trusted.gpg.d/microsoft.gpg
RUN sh -c 'echo "deb [arch=amd64] https://packages.microsoft.com/repos/microsoft-debian-stretch-prod stretch main" > /etc/apt/sources.list.d/dotnetdev.list'
RUN apt-get -qq update
RUN apt-get -qy install dotnet-sdk-2.1.103 libuv1-dev

WORKDIR /usr/src/app

COPY Program.cs Startup.cs aspnetcore.csproj ./

RUN dotnet build --configuration Release

EXPOSE 3000
CMD dotnet run bin/Release/netcoreapp2.0/aspnetcore.dll

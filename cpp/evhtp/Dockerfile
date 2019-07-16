FROM gcc:9.1.0

WORKDIR /usr/src/app
COPY . .

RUN apt -qq update && \
    apt -qy install cmake

RUN cmake -H. -B_builds -DCMAKE_BUILD_TYPE=Release
RUN cmake --build _builds --config Release

EXPOSE 3000
CMD _builds/server_cpp_evhtp

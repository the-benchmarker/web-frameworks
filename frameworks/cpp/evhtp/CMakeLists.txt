cmake_minimum_required(VERSION 3.1)

include("cmake/HunterGate.cmake")
HunterGate(
    URL "https://github.com/ruslo/hunter/archive/v0.20.32.tar.gz"
    SHA1 "72bd88df338cb1b02d4c36c3f85ac87b33578eab"
)

project(server_cpp_evhtp)

hunter_add_package(libevhtp)
find_package(libevhtp CONFIG REQUIRED)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

add_executable(server_cpp_evhtp main.cpp)
target_link_libraries(server_cpp_evhtp libevhtp::evhtp)


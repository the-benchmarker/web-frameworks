cmake_minimum_required(VERSION 3.13)
project(libhv_benchmark)

set(CMAKE_CXX_STANDARD 11)

add_executable(${PROJECT_NAME} main.cpp)

add_subdirectory(libhv)
target_link_libraries(${PROJECT_NAME} PRIVATE libhv)

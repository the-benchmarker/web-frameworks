cmake_minimum_required(VERSION 3.13)
project(nawa_benchmark)

set(CMAKE_CXX_STANDARD 17)

set(BuildExamples OFF CACHE INTERNAL "")
set(BuildDocs OFF CACHE INTERNAL "")
set(BuildSharedLib ON CACHE INTERNAL "")
set(EnableArgon2 OFF CACHE INTERNAL "")

add_executable(${PROJECT_NAME} main.cpp)

add_subdirectory(nawa)
target_link_libraries(${PROJECT_NAME} PRIVATE nawa)

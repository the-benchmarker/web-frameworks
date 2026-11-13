cmake_minimum_required(VERSION 3.6)
project(simple CXX)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(BUILD_SHARED OFF CACHE INTERNAL "")
set(BUILD_STATIC ON CACHE INTERNAL "")
set(BUILD_EXAMPLES OFF CACHE INTERNAL "")
set(WITH_HTTP_CLIENT OFF CACHE INTERNAL "")

add_subdirectory(libhv)

add_executable(${PROJECT_NAME} main.cpp)
target_link_libraries(${PROJECT_NAME} PRIVATE hv_static)

# The image runs /opt/web/simple, copied from the top of the build tree.
set_target_properties(${PROJECT_NAME} PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR})

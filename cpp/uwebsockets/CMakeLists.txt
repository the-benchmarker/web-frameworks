cmake_minimum_required(VERSION 3.9)
project(simple C CXX)

set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# uWebSockets and uSockets build with LTO by default
include(CheckIPOSupported)
check_ipo_supported(RESULT ipo_supported)
if(ipo_supported)
    set(CMAKE_INTERPROCEDURAL_OPTIMIZATION ON)
endif()

find_package(Threads REQUIRED)
find_package(ZLIB REQUIRED)

# uSockets, the same sources its own Makefile compiles, without SSL
set(USOCKETS_DIR ${CMAKE_SOURCE_DIR}/uWebSockets/uSockets)
file(GLOB USOCKETS_SOURCES
        ${USOCKETS_DIR}/src/*.c
        ${USOCKETS_DIR}/src/eventing/*.c
        ${USOCKETS_DIR}/src/crypto/*.c
        ${USOCKETS_DIR}/src/io_uring/*.c)
add_library(uSockets STATIC ${USOCKETS_SOURCES})
target_include_directories(uSockets PUBLIC ${USOCKETS_DIR}/src)
target_compile_definitions(uSockets PUBLIC LIBUS_NO_SSL)

add_executable(${PROJECT_NAME} main.cpp)
target_include_directories(${PROJECT_NAME} PRIVATE ${CMAKE_SOURCE_DIR}/uWebSockets/src)
target_link_libraries(${PROJECT_NAME} PRIVATE uSockets ZLIB::ZLIB Threads::Threads)

# The image runs /opt/web/simple, copied from the top of the build tree.
set_target_properties(${PROJECT_NAME} PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR})

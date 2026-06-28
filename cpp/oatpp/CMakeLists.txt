cmake_minimum_required(VERSION 3.20)

# Project configuration
set(project_name oat_benchmark)
project(${project_name} VERSION 1.0.0 DESCRIPTION "Oatpp Benchmark Application")

# Production-grade C++ settings
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Disable debug and enable production optimizations
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

# Security and optimization flags for production
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wpedantic -Wconversion")
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3 -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} -O2 -g")

# Security hardening flags
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIE -fPIC -fstack-protector-strong -D_FORTIFY_SOURCE=2")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -pie -Wl,-z,now -Wl,-z,relro")

# Disable warnings as errors in release mode (but keep them visible)
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -Wno-error")

# Source files for the library
set(LIB_SOURCES
    src/AppComponent.hpp
    src/controller/MyController.cpp
    src/controller/MyController.hpp
)

# Create the library
add_library(${project_name}-lib ${LIB_SOURCES})

# Find and link oatpp
find_package(oatpp 1.3.0 REQUIRED)

target_link_libraries(${project_name}-lib
    PUBLIC oatpp::oatpp
)

target_include_directories(${project_name}-lib PUBLIC src)

# Set library properties for production
set_target_properties(${project_name}-lib PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED ON
    CXX_EXTENSIONS OFF
    POSITION_INDEPENDENT_CODE ON
    VISIBILITY_INLINES_HIDDEN ON
)

# Add the main executable
add_executable(${project_name} src/App.cpp)

target_link_libraries(${project_name} PRIVATE ${project_name}-lib)
add_dependencies(${project_name} ${project_name}-lib)

# Set executable properties for production
set_target_properties(${project_name} PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED ON
    CXX_EXTENSIONS OFF
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# Enable link-time optimization for release builds
set_target_properties(${project_name} PROPERTIES
    INTERPROCEDURAL_OPTIMIZATION ON
)

# Set output directory for cleaner build structure
set(oatpp_benchmark_output_dir ${CMAKE_BINARY_DIR}/bin)
set_target_properties(${project_name} PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${oatpp_benchmark_output_dir})

# Add version info
target_compile_definitions(${project_name} PRIVATE PROJECT_VERSION="1.0.0")

# Disable console output in release mode (optional)
if(CMAKE_BUILD_TYPE STREQUAL "Release")
    target_compile_definitions(${project_name} PRIVATE OATPP_DISABLE_CONSOLE_LOG)
endif()

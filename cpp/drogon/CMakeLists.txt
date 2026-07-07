cmake_minimum_required(VERSION 3.20)
project(drogon_benchmark)

# Production-grade C++ settings
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Disable debug and enable production optimizations
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

# Security and optimization flags for production
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wpedantic")
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3 -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} -O2 -g")

# Security hardening flags
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIE -fPIC -fstack-protector-strong -D_FORTIFY_SOURCE=2")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -pie -Wl,-z,now -Wl,-z,relro")

# Disable unnecessary features for production
set(BUILD_ORM FALSE CACHE INTERNAL "")
set(BUILD_CTL FALSE CACHE INTERNAL "")
set(BUILD_EXAMPLES FALSE CACHE INTERNAL "")
set(BUILD_TESTS FALSE CACHE INTERNAL "")

# Source files
set(sources main.cc plugins/SyncPlugin.cc controllers/UserCtrl.cc)

# Create executable
add_executable(${PROJECT_NAME} ${sources})

# Link libraries
add_subdirectory(drogon)
target_link_libraries(${PROJECT_NAME} PRIVATE drogon)

# Set output directory for cleaner build structure
set(drogon_benchmark_output_dir ${CMAKE_BINARY_DIR}/bin)
set_target_properties(${PROJECT_NAME} PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${drogon_benchmark_output_dir})

cmake_minimum_required (VERSION 3.2)
project(drogon_benchmark)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(BUILD_ORM FALSE CACHE INTERNAL "")
set(BUILD_CTL FALSE CACHE INTERNAL "")
set(BUILD_EXAMPLES FALSE CACHE INTERNAL "")

add_subdirectory(drogon)
set(Drogon_DIR ${PROJECT_BINARY_DIR}/drogon)
FIND_PACKAGE(Drogon CONFIG REQUIRED NO_DEFAULT_PATH)
include_directories(${DROGON_INCLUDE_DIRS})
link_libraries(${DROGON_LIBRARIES})

set(sources main.cc 
            controllers/BenchmarkCtrl.cc
            controllers/UserCtrl.cc)

add_executable(${PROJECT_NAME} ${sources})

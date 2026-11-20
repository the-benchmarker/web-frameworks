cmake_minimum_required(VERSION 3.2)
project(drogon_benchmark)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(BUILD_ORM FALSE CACHE INTERNAL "")
set(BUILD_CTL FALSE CACHE INTERNAL "")
set(BUILD_EXAMPLES FALSE CACHE INTERNAL "")

set(sources main.cc plugins/SyncPlugin.cc controllers/UserCtrl.cc)

add_executable(${PROJECT_NAME} ${sources})

add_subdirectory(drogon)
target_link_libraries(${PROJECT_NAME} PRIVATE drogon)

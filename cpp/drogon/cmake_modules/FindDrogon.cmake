#
# FindDrogon submodule and setup benchmark cmake target 
#

message(STATUS "Searching for Drogon")

add_library(Drogon::Drogon INTERFACE IMPORTED GLOBAL)
find_package (Jsoncpp REQUIRED)

set_target_properties(Drogon::Drogon PROPERTIES
      INTERFACE_INCLUDE_DIRECTORIES 
      "${CMAKE_CURRENT_SOURCE_DIR}/drogon/lib/inc;${CMAKE_CURRENT_SOURCE_DIR}/drogon/orm_lib/inc;${CMAKE_CURRENT_SOURCE_DIR}/drogon/trantor;${JSONCPP_INCLUDE_DIRS};")

target_link_libraries(Drogon::Drogon INTERFACE
    drogon trantor dl pthread)

message(STATUS "Drogon::Drogon interface target defined")

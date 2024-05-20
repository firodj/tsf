cmake_minimum_required (VERSION 3.8)
project (tsf)

set(CMAKE_C_STANDARD 17)

list(APPEND TSF_SOURCES tsf.c tml.c)

add_library(tsf STATIC ${TSF_SOURCES})

# target_compile_definitions(tsf PUBLIC ...)
target_include_directories(tsf PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_compile_definitions(tsf
    PUBLIC
        # If the debug configuration pass the DEBUG define to the compiler
        $<$<CONFIG:Debug>:DEBUG>
)

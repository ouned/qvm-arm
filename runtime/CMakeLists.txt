cmake_minimum_required(VERSION 2.8)

project(runtime)

if(WIN32)
        if(CMAKE_SIZEOF_VOID_P MATCHES "8")
            set(AMD64 ON)
        else()
            set(I386 ON)
        endif()
else()
        if(CMAKE_SYSTEM_PROCESSOR MATCHES "^arm")
            set(ARM ON)
        elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "^i.86$")
            set(I386 ON)
        elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "^x86.64$")
            set(AMD64 ON)
        endif()
endif()

set(FILES
    "main.h"
    "interpreter.h"
    "compiler.h"

    "main.c"
    "interpreter.c"
)

if(I386)
    set(FILES ${FILES} "compiler_i386.c")
elseif(ARM)
    set(FILES ${FILES} "compiler_arm.c")
endif()

add_executable(${PROJECT_NAME} ${FILES})

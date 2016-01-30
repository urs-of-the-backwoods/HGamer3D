#	CMakeLists.txt file for game giornata (Urho3D Binding)
#	HGamer3D Library (A project to enable 3D game development in Haskell)
#	Copyright 2016 Peter Althainz
#	
#	Distributed under the Apache License, Version 2.0
#	(See attached file LICENSE or copy at 
#	http://www.apache.org/licenses/LICENSE-2.0)
#
#	file: fresco/game-giornata/CMakeLists.txt


# Include general info
cmake_minimum_required (VERSION 2.8)

#
# basic project information
#

project (game-giornata)
set (game-giornata_VERSION_MAJOR 0)
set (game-giornata_VERSION_MINOR 7)
set (game-giornata_VERSION_PATCH 0)

# strategy of this cmake file
# ---------------------------
# The target: game_gio_lib is the c++ dll for the urho3d binding
# It depends on the files:
#    cpp source files
# It needs:
#    Urho3D libraries
#

#
# needed libraries
#

# MessagePack
include_directories (${CMAKE_CURRENT_SOURCE_DIR}/msgpack-c/include)

#
# files considered during build of target
#

set(CPP_SRCS
	interface.cpp 
	graphics3d.cpp
	input.cpp
    gui.cpp
    audio.cpp
)

include(GenerateExportHeader)

add_library(game_gio_lib SHARED ${CPP_SRCS})
generate_export_header(game_gio_lib)
#set_target_properties(game_gio_lib PROPERTIES OUTPUT_NAME ${DLLNAME})
include_directories(${CMAKE_CURRENT_BINARY_DIR})

# Urho3D

# Set CMake modules search path and Urho home, find the rest

IF(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
  set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "C:/Program Files (x86)/Urho3D/share/Urho3D/CMake/Modules")
  set (URHO3D_HOME "C:/Program Files (x86)/Urho3D")
ENDIF(${CMAKE_SYSTEM_NAME} MATCHES "Windows")

IF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "/usr/local/share/Urho3D/CMake/Modules")
  set (URHO3D_HOME "/usr/local/share/Urho3D")
ENDIF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")

find_package (Urho3D REQUIRED)
include (Urho3D-CMake-common)
include_directories (${URHO3D_INCLUDE_DIRS})

# target, library
set(TARGET_NAME game_gio_lib)
define_dependency_libs(Urho3D)
target_link_libraries(game_gio_lib ${ABSOLUTE_PATH_LIBS} ${LIBS})

#
# add custom target to install in Output build folder
#

add_custom_target(output_dll DEPENDS game_gio_lib)
add_custom_command(TARGET output_dll
                COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:game_gio_lib> ${HG3D_RUNTIME_DIR}/$<TARGET_FILE_NAME:game_gio_lib>
                DEPENDS game_gio_lib
)


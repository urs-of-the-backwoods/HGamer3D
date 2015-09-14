#	CMakeLists.txt file for Urho3D Binding
#	HGamer3D Library (A project to enable 3D game development in Haskell)
#	Copyright 2015 Peter Althainz
#	
#	Distributed under the Apache License, Version 2.0
#	(See attached file LICENSE or copy at 
#	http://www.apache.org/licenses/LICENSE-2.0)
#
#	file: Urho3D-Binding/CMakeLists.txt


# Set minimum version
cmake_minimum_required (VERSION 2.8.6)

#
# basic project information
#

project (Urho3DBinding)

# transfer to parent scope
set (Urho3DBinding_VERSION_MAJOR ${HGamer3D_VERSION_MAJOR} )
set (Urho3DBinding_VERSION_MINOR ${HGamer3D_VERSION_MINOR} )
set (Urho3DBinding_VERSION_PATCH ${HGamer3D_VERSION_PATCH} )

set (Urho3DBinding_VERSION_MAJOR ${Urho3DBinding_VERSION_MAJOR} PARENT_SCOPE)
set (Urho3DBinding_VERSION_MINOR ${Urho3DBinding_VERSION_MINOR} PARENT_SCOPE)
set (Urho3DBinding_VERSION_PATCH ${Urho3DBinding_VERSION_PATCH} PARENT_SCOPE)

set (DLLNAME hgamer3d${HGamer3D_VERSION_MAJOR}${HGamer3D_VERSION_MINOR}${HGamer3D_VERSION_PATCH})

# strategy of this cmake file
# ---------------------------
# The target: urho3dbinding_clib is the c++ dll for the urho3d binding
# It depends on the files:
#    cpp source files
# It needs:
#    Urho3D libraries
#

#
# needed libraries
#

# MessagePack
include_directories (${CMAKE_CURRENT_SOURCE_DIR}/../ThirdParty/msgpack-c/include)

#
# files considered during build of target
#

set(CPP_SRCS
	interface.cpp 
	graphics3d.cpp
	input.cpp
)

include(GenerateExportHeader)

add_library(urho3dbinding_clib SHARED ${CPP_SRCS})
generate_export_header(urho3dbinding_clib)
set_target_properties(urho3dbinding_clib PROPERTIES OUTPUT_NAME ${DLLNAME})
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
set(TARGET_NAME urho3dbinding_clib)
define_dependency_libs(Urho3D)
target_link_libraries(urho3dbinding_clib ${ABSOLUTE_PATH_LIBS} ${LIBS})

#
# add custom target to install in Output build folder
#

add_custom_target(output_dll DEPENDS urho3dbinding_clib)
add_custom_command(TARGET output_dll
                COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:urho3dbinding_clib> ${HG3D_RUNTIME_DIR}/$<TARGET_FILE_NAME:urho3dbinding_clib>
                DEPENDS urho3dbinding_clib
)


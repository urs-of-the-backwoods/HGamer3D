# This source file is part of HGamer3D
# (A project to enable 3D game development in Haskell)
# For the latest info, see http://www.hgamer3d.org
#
# (c) 2015 Peter Althainz
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# CMakeLists.txt for Urho3D-Binding
# ---------------------------------

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
set_target_properties(urho3dbinding_clib PROPERTIES OUTPUT_NAME hgamer3d${HGamer3D_VERSION_MAJOR})
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
                COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:urho3dbinding_clib> ${CMAKE_BINARY_DIR}/Output/$<TARGET_FILE_NAME:urho3dbinding_clib>
                DEPENDS urho3dbinding_clib
)


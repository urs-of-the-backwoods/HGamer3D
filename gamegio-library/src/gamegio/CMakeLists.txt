#	CMakeLists.txt file for game giornata (Urho3D Binding)
#	HGamer3D Library (A project to enable 3D game development in Haskell)
#	Copyright 2016 - 2017 Peter Althainz
#	
#	Distributed under the Apache License, Version 2.0
#	(See attached file LICENSE or copy at 
#	http://www.apache.org/licenses/LICENSE-2.0)
#
#	file: fresco/game-giornata/CMakeLists.txt


# Set minimum version
cmake_minimum_required (VERSION 2.8.6)

#
# basic project information
#

# History
# 1.0.2 build with arch=core2
# 2.0.0 changed to new architecture with tinycbor serilisation

project (game-giornata)
set (game-giornata_VERSION_MAJOR 2)
set (game-giornata_VERSION_MINOR 0)
set (game-giornata_VERSION_PATCH 0)

if (COMMAND cmake_policy)
    cmake_policy (SET CMP0003 NEW)
    if (CMAKE_VERSION VERSION_GREATER 2.8.12 OR CMAKE_VERSION VERSION_EQUAL 2.8.12)
        # INTERFACE_LINK_LIBRARIES defines the link interface
        cmake_policy (SET CMP0022 NEW)
    endif ()
    if (CMAKE_VERSION VERSION_GREATER 3.0.0 OR CMAKE_VERSION VERSION_EQUAL 3.0.0)
        # Disallow use of the LOCATION target property - therefore we set to OLD as we still need it
        cmake_policy (SET CMP0026 OLD)
        # MACOSX_RPATH is enabled by default
        cmake_policy (SET CMP0042 NEW)
    endif ()
endif ()






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

# tinycbor
include_directories (${CMAKE_CURRENT_SOURCE_DIR}/../../tinycbor/build/tinycbor/src)

#
# files considered during build of target
#

set(CPP_SRCS
    Fresco.cpp
    Library.cpp
    LMHCbor.cpp
    Graphics3DConfigCbor.cpp
    Graphics3DCommandCbor.cpp
    SoundSourceCbor.cpp
    PlayCmdCbor.cpp
    Vec3Cbor.cpp
    EntityIdCbor.cpp
    ParentCbor.cpp
    UnitQuaternionCbor.cpp
    InputEventHandlerCbor.cpp
    KeyEventCbor.cpp
    MouseCbor.cpp
    SoundListenerCbor.cpp
    VolumeCbor.cpp
    VisibleCbor.cpp
    CameraCbor.cpp
    ColourCbor.cpp
    LightCbor.cpp
    MaterialCbor.cpp
    GeometryCbor.cpp
    AngleCbor.cpp
    ScreenRectCbor.cpp
    Graphics3DSystem.cpp
    HasNode.cpp
    VolumeItem.cpp
    SoundSourceItem.cpp
    SoundListenerItem.cpp
    InputEventHandler.cpp
    CameraItem.cpp
    LightItem.cpp
    GeometryItem.cpp
    GUIElements.cpp
    Slider2.cpp
    LineEdit2.cpp
    ButtonCbor.cpp
    CheckBoxCbor.cpp
    DropDownListCbor.cpp
    EditTextCbor.cpp
    SliderCbor.cpp
    StaticTextCbor.cpp
    UIElementCbor.cpp
)

include(GenerateExportHeader)

add_library(game_gio_lib SHARED ${CPP_SRCS})
generate_export_header(game_gio_lib)
#set_target_properties(game_gio_lib PROPERTIES OUTPUT_NAME ${DLLNAME})
include_directories(${CMAKE_CURRENT_BINARY_DIR})



# Urho3D

# Set CMake modules search path
set (CMAKE_MODULE_PATH ${URHO3D_SRC}/CMake/Modules)

# Include Urho3D Cmake common module
include (Urho3D-CMake-common)

# Find Urho3D library
find_package (Urho3D REQUIRED)
include_directories (${URHO3D_INCLUDE_DIRS})

# add specific C Flags

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -march=core2")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -march=core2")

# target, library
set(TARGET_NAME game_gio_lib)
define_dependency_libs(Urho3D)
target_link_libraries(game_gio_lib ${ABSOLUTE_PATH_LIBS} ${LIBS})

target_link_libraries(game_gio_lib ${CMAKE_CURRENT_SOURCE_DIR}/../../tinycbor/build/tinycbor/lib/libtinycbor.a)

#
# add custom target to install in Output build folder
#

add_custom_target(output_dll DEPENDS game_gio_lib)
add_custom_command(TARGET output_dll
                COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:game_gio_lib> ${HG3D_RUNTIME_DIR}/$<TARGET_FILE_NAME:game_gio_lib>
                DEPENDS game_gio_lib
)


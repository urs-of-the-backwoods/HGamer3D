#	CMakeLists.txt file for HGamer3D - gamegio-library
#	Copyright 2016 - 2017 Peter Althainz
#	
#	Distributed under the Apache License, Version 2.0
#	(See attached file LICENSE or copy at 
#	http://www.apache.org/licenses/LICENSE-2.0)
#
#	file: HGamer3D/gamegio-library/src/CMakeLists.txt


# Set minimum version
cmake_minimum_required (VERSION 2.8.6)

#
# basic project information
#

# History
# 1.0.2 build with arch=core2
# 2.0.0 changed to new architecture with tinycbor serialisation

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
# files considered during build of target
#

set(CPP_SRCS
    gamegio/Fresco.cpp
    gamegio/Library.cpp
    gamegio/LMHCbor.cpp
    gamegio/Graphics3DConfigCbor.cpp
    gamegio/Graphics3DCommandCbor.cpp
    gamegio/SoundSourceCbor.cpp
    gamegio/PlayCmdCbor.cpp
    gamegio/Vec3Cbor.cpp
    gamegio/EntityIdCbor.cpp
    gamegio/ParentCbor.cpp
    gamegio/UnitQuaternionCbor.cpp
    gamegio/InputEventHandlerCbor.cpp
    gamegio/KeyEventCbor.cpp
    gamegio/MouseCbor.cpp
    gamegio/SoundListenerCbor.cpp
    gamegio/VolumeCbor.cpp
    gamegio/VisibleCbor.cpp
    gamegio/CameraCbor.cpp
    gamegio/ColourCbor.cpp
    gamegio/LightCbor.cpp
    gamegio/MaterialCbor.cpp
    gamegio/GeometryCbor.cpp
    gamegio/AngleCbor.cpp
    gamegio/ScreenRectCbor.cpp
    gamegio/Graphics3DSystem.cpp
    gamegio/HasNode.cpp
    gamegio/VolumeItem.cpp
    gamegio/SoundSourceItem.cpp
    gamegio/SoundListenerItem.cpp
    gamegio/InputEventHandler.cpp
    gamegio/CameraItem.cpp
    gamegio/LightItem.cpp
    gamegio/GeometryItem.cpp
    gamegio/GUIElements.cpp
    gamegio/DropDownListItem.cpp
    gamegio/Slider2.cpp
    gamegio/LineEdit2.cpp
    gamegio/ButtonCbor.cpp
    gamegio/CheckBoxCbor.cpp
    gamegio/DropDownListCbor.cpp
    gamegio/EditTextCbor.cpp
    gamegio/SliderCbor.cpp
    gamegio/StaticTextCbor.cpp
    gamegio/UIElementCbor.cpp
    gamegio/Joystick.cpp
    gamegio/JoystickCbor.cpp
    gamegio/SceneItem.cpp
    gamegio/SceneItemCbor.cpp
    gamegio/SkyboxItem.cpp
    gamegio/SkyboxItemCbor.cpp
    gamegio/SpriteItemCbor.cpp
    gamegio/Text3DItemCbor.cpp
    gamegio/Text3DItem.cpp
    gamegio/LabelCbor.cpp
)

set(CBOR_SRCS
    third-party/tinycbor/src/cborencoder.c
    third-party/tinycbor/src/cborencoder_close_container_checked
    third-party/tinycbor/src/cborerrorstrings.c
    third-party/tinycbor/src/cborparser.c
    third-party/tinycbor/src/cborparser_dup_string.c
    third-party/tinycbor/src/cborpretty.c
#    third-party/tinycbor/src/cbortojson.c
#    third-party/tinycbor/src/open_memstream.c
)

include(GenerateExportHeader)

add_library(game_gio_lib SHARED ${CPP_SRCS} ${CBOR_SRCS})
generate_export_header(game_gio_lib)
#set_target_properties(game_gio_lib PROPERTIES OUTPUT_NAME ${DLLNAME})

include_directories(${CMAKE_CURRENT_BINARY_DIR})
include_directories($ENV{URHO3D_HOME}/include $ENV{URHO3D_HOME}/include/Urho3D/ThirdParty)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/gamegio ${CMAKE_CURRENT_SOURCE_DIR}/third-party/tinycbor/src)
if(WIN32)
	target_link_libraries(game_gio_lib $ENV{URHO3D_HOME}/lib/Urho3D.lib)
elseif(APPLE)
	target_link_libraries(game_gio_lib $ENV{URHO3D_HOME}/lib/libUrho3D.dylib)
else()
	target_link_libraries(game_gio_lib $ENV{URHO3D_HOME}/lib/libUrho3D.so)
endif()

# add specific C Flags

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -march=core2")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -march=core2")

# target, library
#set(TARGET_NAME game_gio_lib)

#
# add custom target to install in Output build folder
#

add_custom_target(output_dll DEPENDS game_gio_lib)
add_custom_command(TARGET output_dll
                COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:game_gio_lib> ${HG3D_RUNTIME_DIR}/$<TARGET_FILE_NAME:game_gio_lib>
                DEPENDS game_gio_lib
)


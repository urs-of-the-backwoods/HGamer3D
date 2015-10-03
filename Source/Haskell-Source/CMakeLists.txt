#	CMakeLists.txt file for Haskell Sources
#	HGamer3D Library (A project to enable 3D game development in Haskell)
#	Copyright 2015 Peter Althainz
#	
#	Distributed under the Apache License, Version 2.0
#	(See attached file LICENSE or copy at 
#	http://www.apache.org/licenses/LICENSE-2.0)
#
#	file: Haskell-Source/CMakeLists.txt

cmake_minimum_required (VERSION 2.8.6)

#
# basic project information
#

project (HaskellPackage)

# strategy of this cmake file
# ---------------------------
# The target: hgamer3d_hs_tgz is the haskell source file archive, which is the main target.
# It depends on the targets: 
# 	 -
# It depends on the files:
#    cabal file (configured)
#    static files (LICENSE, ...)
#    Haskell source files
#

# the final target archive file 
set(HGAMER3D_TGZ HGamer3D-${HGamer3D_VERSION_MAJOR}.${HGamer3D_VERSION_MINOR}.${HGamer3D_VERSION_PATCH}.tar.gz)

#
# Files to be copied from SRC to BINARY folder, configuration, statics, sources
#

# cabal configuration file for the target package
configure_file(Library/HGamer3D.cabal ${CMAKE_CURRENT_BINARY_DIR}/Library/HGamer3D.cabal)
configure_file(Samples/HGamer3D-SamplePackage.cabal ${CMAKE_CURRENT_BINARY_DIR}/Samples/HGamer3D-SamplePackage.cabal)
configure_file(stack.yaml ${CMAKE_CURRENT_BINARY_DIR}/stack.yaml)

# static files
set(STATIC_FILES 
	Library/LICENSE 
	Library/Setup.hs 
	Samples/LICENSE 
	Samples/Setup.hs 
	)
foreach(file ${STATIC_FILES})
	add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${file}
		COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/${file} ${CMAKE_CURRENT_BINARY_DIR}/${file}
		DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${file}
	)
	set(STATIC_BIN_FILES ${STATIC_BIN_FILES} ${CMAKE_CURRENT_BINARY_DIR}/${file})
endforeach()

# Haskell source files
set(HASKELL_SRCS
	Library/HGamer3D/Binding/Helper.hs
	Library/HGamer3D/Binding.hs
	Library/HGamer3D/Data/Component.hs
	Library/HGamer3D/Data/Angle.hs
	Library/HGamer3D/Data/Colour.hs
	Library/HGamer3D/Data/LMH.hs
	Library/HGamer3D/Data/GameTime.hs
	Library/HGamer3D/Data/Geometry2D.hs
	Library/HGamer3D/Data/Transform3D.hs
	Library/HGamer3D/Data/TypeSynonyms.hs
	Library/HGamer3D/Data/Vector.hs
	Library/HGamer3D/Data/Window.hs
	Library/HGamer3D/Data/PlayCmd.hs
	Library/HGamer3D/Data.hs
	Library/HGamer3D/Util/FileLocation.hs
	Library/HGamer3D/Util/UniqueName.hs
	Library/HGamer3D/Util.hs
	Library/HGamer3D/ECS/Entity.hs
	Library/HGamer3D/ECS/System.hs
	Library/HGamer3D/ECS.hs
	Library/HGamer3D/Graphics3D.hs
	Library/HGamer3D/Graphics3D/Camera.hs
	Library/HGamer3D/Graphics3D/Geometry.hs
	Library/HGamer3D/Graphics3D/Light.hs
	Library/HGamer3D/Graphics3D/Material.hs
	Library/HGamer3D/Graphics3D/Window.hs
	Library/HGamer3D/Graphics3D/Graphics3DCommand.hs
	Library/HGamer3D/Graphics3D/Graphics3DConfig.hs
	Library/HGamer3D/Graphics3D/SystemGraphics3D.hs
	Library/HGamer3D/Input/Mouse.hs
	Library/HGamer3D/Input/Keyboard.hs
	Library/HGamer3D/Input/Joystick.hs
	Library/HGamer3D/Input/InputEventHandler.hs
	Library/HGamer3D/Input.hs
    Library/HGamer3D/GUI/Button.hs
    Library/HGamer3D/GUI/EditText.hs
    Library/HGamer3D/GUI/Text.hs
    Library/HGamer3D/GUI/Slider.hs
    Library/HGamer3D/GUI/CheckBox.hs
    Library/HGamer3D/GUI/DropDownList.hs
    Library/HGamer3D/GUI.hs
    Library/HGamer3D/Audio/SoundSource.hs
    Library/HGamer3D/Audio/SoundListener.hs
    Library/HGamer3D/Audio/Volume.hs
    Library/HGamer3D/Audio.hs
	Library/HGamer3D.hs
)
foreach(file ${HASKELL_SRCS})
	add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${file}
		COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/${file} ${CMAKE_CURRENT_BINARY_DIR}/${file}
		DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${file}
	)
	set(HASKELL_SRCS_BIN_FILES ${HASKELL_SRCS_BIN_FILES} ${CMAKE_CURRENT_BINARY_DIR}/${file})
endforeach()

set (SAMPLE_SRCS
	Samples/RotatingCube.hs
	Samples/Materials.hs		
	Samples/engineShutdown.hs
    Samples/Cuboid2.hs
    Samples/Gui1.hs
    Samples/SoundEffects.hs
)
foreach(file ${SAMPLE_SRCS})
	add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${file}
		COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/${file} ${CMAKE_CURRENT_BINARY_DIR}/${file}
		DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${file}
	)
	set(SAMPLE_SRCS_BIN_FILES ${SAMPLE_SRCS_BIN_FILES} ${CMAKE_CURRENT_BINARY_DIR}/${file})
endforeach()


#
# special targets
#

# c2hs generated interface file
set(BIN_INTERFACE_HS ${CMAKE_CURRENT_BINARY_DIR}/Library/HGamer3D/Binding/CFunctions.hs)
# handle the interface generation with c2hs
add_custom_command(OUTPUT ${BIN_INTERFACE_HS}
				COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/interface.h interface.h
				COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/CFunctions.chs CFunctions.chs
				COMMAND stack exec c2hs -- interface.h CFunctions.chs
				COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/CFunctions.hs Library/HGamer3D/Binding/CFunctions.hs
				COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/CFunctions.chs.h Library/HGamer3D/Binding/CFunctions.chs.h
				)


#
# final executables
#

# executable files 

foreach(file ${SAMPLE_SRCS})
	get_filename_component(fname ${file} NAME_WE)
    IF(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
        set(SAMPLE_EXES ${SAMPLE_EXES} ${CMAKE_CURRENT_BINARY}/${fname}.exe)
    ENDIF(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    IF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
        set(SAMPLE_EXES  ${CMAKE_CURRENT_BINARY}/${fname})    # intentionally, last exe only will be put into the SAMPLE_EXES for Linux (make file target)
    ENDIF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
endforeach()

add_custom_command(OUTPUT ${SAMPLE_EXES}
	           	COMMAND stack build 
	           	DEPENDS ${STATIC_BIN_FILES} ${SAMPLE_SRCS_BIN_FILES} ${HASKELL_SRCS_BIN_FILES} ${BIN_INTERFACE_HS}
		   )
add_custom_target(samples DEPENDS ${SAMPLE_EXES} output_dll)


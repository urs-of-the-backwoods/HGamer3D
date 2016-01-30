#	CMakeLists.txt file for copying runtime files
#	HGamer3D Library (A project to enable 3D game development in Haskell)
#	Copyright 2015 Peter Althainz
#	
#	Distributed under the Apache License, Version 2.0
#	(See attached file LICENSE or copy at 
#	http://www.apache.org/licenses/LICENSE-2.0)
#
#	file: Runtime/CMakeLists.txt


cmake_minimum_required (VERSION 2.8)

project (Runtime)

# copy runtime sourcefiles
#

set (DATA_FILES
    README_RUNTIME.txt
    Data/Materials/ColourAqua.xml
    Data/Materials/ColourBlack.xml
    Data/Materials/ColourBlue.xml
    Data/Materials/ColourDarkGrey.xml
    Data/Materials/ColourFuchsia.xml
    Data/Materials/ColourGreen.xml
    Data/Materials/ColourGrey.xml
    Data/Materials/ColourLime.xml
    Data/Materials/ColourMaroon.xml
    Data/Materials/ColourNavy.xml
    Data/Materials/ColourOlive.xml
    Data/Materials/ColourPurple.xml
    Data/Materials/ColourRed.xml
    Data/Materials/ColourSilver.xml
    Data/Materials/ColourTeal.xml
    Data/Materials/ColourWhite.xml
    Data/Materials/ColourYellow.xml
    Data/Materials/Pattern_01.xml
    Data/Materials/Pattern_03.xml
    Data/Materials/Pattern_04.xml
    Data/Materials/Pattern_100.xml
    Data/Materials/Pattern_102.xml
    Data/Materials/Pattern_12.xml
    Data/Materials/Pattern_124.xml
    Data/Materials/Pattern_125.xml
    Data/Materials/Pattern_126.xml
    Data/Materials/Pattern_127.xml
    Data/Materials/Pattern_13.xml
    Data/Materials/Pattern_14.xml
    Data/Materials/Pattern_15.xml
    Data/Materials/Pattern_270.xml
    Data/Materials/Pattern_271.xml
    Data/Materials/Pattern_272.xml
    Data/Materials/Pattern_273.xml
    Data/Materials/Pattern_29.xml
    Data/Materials/Pattern_30.xml
    Data/Materials/Pattern_31.xml
    Data/Materials/Pattern_42.xml
    Data/Materials/Pattern_43.xml
    Data/Materials/Pattern_44.xml
    Data/Materials/Pattern_45.xml
    Data/Materials/Pattern_46.xml
)

foreach(file ${DATA_FILES})
	add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/../Haskell-Source/${file}
		COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/${file} ${CMAKE_CURRENT_BINARY_DIR}/../Haskell-Source/${file}
		DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${file}
	)
	set(DATA_BIN_FILES ${DATA_BIN_FILES} ${CMAKE_CURRENT_BINARY_DIR}/../Haskell-Source/${file})
endforeach()

# copy runtime binaries
add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/../Haskell-Source/CoreData/D3Dcompiler_47.dll
    COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/../../Runtime-Binaries/CoreData ${file} ${CMAKE_CURRENT_BINARY_DIR}/../Haskell-Source/CoreData
    COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/../../Runtime-Binaries/Data ${file} ${CMAKE_CURRENT_BINARY_DIR}/../Haskell-Source/Data
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/../../Runtime-Binaries/D3Dcompiler_47.dll ${CMAKE_CURRENT_BINARY_DIR}/../Haskell-Source/D3Dcompiler_47.dll
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${file}
)

# set custom target for the file copy operation
add_custom_target(runtime DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/../Haskell-Source/CoreData/D3Dcompiler_47.dll ${DATA_BIN_FILES})



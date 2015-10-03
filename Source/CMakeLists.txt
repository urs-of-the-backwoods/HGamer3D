#	Main CMakeLists.txt file for HGamer3D
#	HGamer3D Library (A project to enable 3D game development in Haskell)
#	Copyright 2015 Peter Althainz
#	
#	Distributed under the Apache License, Version 2.0
#	(See attached file LICENSE or copy at 
#	http://www.apache.org/licenses/LICENSE-2.0)
#
#	file: CMakeLists.txt

# special targets:
# output_dll (copies runtime dll to HG3D_RUNTIME_DIR)
# samples (builds the samples)
# runtime (copy's runtime info to folder)

cmake_minimum_required (VERSION 2.8)

# basic project information
project (HGamer3D)
set (HGamer3D_VERSION_MAJOR 0)
set (HGamer3D_VERSION_MINOR 6)
set (HGamer3D_VERSION_PATCH 2)

# Runtime DLL folder
IF(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    set (HG3D_RUNTIME_DIR "C:/HGamer3D_RT")
ENDIF(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
IF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    set (HG3D_RUNTIME_DIR /usr/local/lib/Urho3D)
ENDIF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")

# Subdirectories
add_subdirectory(Urho3D-Binding)
add_subdirectory(Haskell-Source)
add_subdirectory(Runtime)

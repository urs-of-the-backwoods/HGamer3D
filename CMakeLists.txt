#	Main CMakeLists.txt file for HGamer3D
#	HGamer3D Library (A project to enable 3D game development in Haskell)
#	Copyright 2015 Peter Althainz
#	
#	Distributed under the Apache License, Version 2.0
#	(See attached file LICENSE or copy at 
#	http://www.apache.org/licenses/LICENSE-2.0)
#
#	file: CMakeLists.txt

# basic project information
set (HGamer3D_VERSION_MAJOR 0)
set (HGamer3D_VERSION_MINOR 7)
set (HGamer3D_VERSION_PATCH 0)

# Runtime DLL folder
IF(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    set (HG3D_RUNTIME_DIR "C:/HGamer3D_RT")
    set (URHO3D_RUNTIME_DIR "")
ENDIF(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
IF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    set (HG3D_RUNTIME_DIR /usr/local/lib)
    set (URHO3D_RUNTIME_DIR /usr/local/lib/Urho3D)
ENDIF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")

# transfer to parent scope
set (Urho3DBinding_VERSION_MAJOR ${HGamer3D_VERSION_MAJOR} )
set (Urho3DBinding_VERSION_MINOR ${HGamer3D_VERSION_MINOR} )
set (Urho3DBinding_VERSION_PATCH ${HGamer3D_VERSION_PATCH} )

set (Urho3DBinding_VERSION_MAJOR ${Urho3DBinding_VERSION_MAJOR} PARENT_SCOPE)
set (Urho3DBinding_VERSION_MINOR ${Urho3DBinding_VERSION_MINOR} PARENT_SCOPE)
set (Urho3DBinding_VERSION_PATCH ${Urho3DBinding_VERSION_PATCH} PARENT_SCOPE)

set (DLLNAME hgamer3d${HGamer3D_VERSION_MAJOR}${HGamer3D_VERSION_MINOR}${HGamer3D_VERSION_PATCH})

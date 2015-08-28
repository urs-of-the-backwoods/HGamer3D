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

# CMakeLists.txt Haskell-Source
# -----------------------------

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

#
# files considered during build of the target
#

# the final target archive file 
set(HGAMER3D_TGZ HGamer3D-${HGamer3D_VERSION_MAJOR}.${HGamer3D_VERSION_MINOR}.${HGamer3D_VERSION_PATCH}.tar.gz)

# cabal configuration file for the target package
configure_file(HGamer3D.cabal ${CMAKE_CURRENT_BINARY_DIR})

# static files
set(STATIC_FILES LICENSE Setup.hs)
foreach(file ${STATIC_FILES})
	add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${file}
		COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/${file} ${CMAKE_CURRENT_BINARY_DIR}/${file}
		DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${file}
	)
	set(STATIC_BIN_FILES ${STATIC_BIN_FILES} ${CMAKE_CURRENT_BINARY_DIR}/${file})
endforeach()

# Haskell source files
set(HASKELL_SRCS
	HGamer3D/Binding/CFunctions.hs
	HGamer3D/Binding/Helper.hs
	HGamer3D/Binding.hs
	HGamer3D/Data/Component.hs
	HGamer3D/Data/Angle.hs
	HGamer3D/Data/Colour.hs
	HGamer3D/Data/LMH.hs
	HGamer3D/Data/GameTime.hs
	HGamer3D/Data/Geometry2D.hs
	HGamer3D/Data/Transform3D.hs
	HGamer3D/Data/TypeSynonyms.hs
	HGamer3D/Data/Vector.hs
	HGamer3D/Data/Window.hs
	HGamer3D/Data.hs
	HGamer3D/Util/FileLocation.hs
	HGamer3D/Util/UniqueName.hs
	HGamer3D/Util.hs
	HGamer3D/ECS/Entity.hs
	HGamer3D/ECS/System.hs
	HGamer3D/ECS.hs
	HGamer3D/Graphics3D/Camera.hs
	HGamer3D/Graphics3D/Geometry.hs
	HGamer3D/Graphics3D/Light.hs
	HGamer3D/Graphics3D/Material.hs
	HGamer3D/Graphics3D/Window.hs
	HGamer3D/Graphics3D/Graphics3DCommand.hs
	HGamer3D/Graphics3D/Graphics3DConfig.hs
	HGamer3D/Graphics3D/SystemGraphics3D.hs
	HGamer3D/Input/InputEvents.hs
	HGamer3D/Input.hs
	HGamer3D.hs
)

# c2hs generated interface file
set(BIN_INTERFACE_HS ${CMAKE_CURRENT_BINARY_DIR}/HGamer3D/Binding/CFunctions.hs)

#
# custom commands and targets
#

# handle the interface generation with c2hs
add_custom_command(OUTPUT ${BIN_INTERFACE_HS}
				COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/interface.h interface.h
				COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/CFunctions.chs CFunctions.chs
				COMMAND c2hs interface.h CFunctions.chs
				COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/CFunctions.hs HGamer3D/Binding/CFunctions.hs
				COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/CFunctions.chs.h HGamer3D/Binding/CFunctions.chs.h
				)

# create source distribution
add_custom_command(OUTPUT dist/${HGAMER3D_TGZ}
				COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/HGamer3D.hs HGamer3D.hs
	           	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/HGamer3D HGamer3D
	           	COMMAND cabal sdist
	           	DEPENDS ${STATIC_BIN_FILES} ${HASKELL_SRCS} ${BIN_INTERFACE_HS}
		   )
add_custom_target(hgamer3d_hs_tgz ALL DEPENDS dist/${HGAMER3D_TGZ})

#
# add custom target to install in Output build folder
#

add_custom_target(output_tgz DEPENDS ${CMAKE_BINARY_DIR}/Output/${HGAMER3D_TGZ})
add_custom_command(OUTPUT ${CMAKE_BINARY_DIR}/Output/${HGAMER3D_TGZ}
				COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/dist/${HGAMER3D_TGZ} ${CMAKE_BINARY_DIR}/Output/${HGAMER3D_TGZ}
				DEPENDS hgamer3d_hs_tgz ${CMAKE_CURRENT_BINARY_DIR}/dist/${HGAMER3D_TGZ}
)



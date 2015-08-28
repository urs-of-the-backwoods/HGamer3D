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

# CMakeLists.txt Tests
# --------------------

cmake_minimum_required (VERSION 2.8.6)


#
# basic project information
#

project (Samples)

#
# cabal file, hgamer3d tgz
#

configure_file(HGamer3D-SamplePackage.cabal ${CMAKE_CURRENT_BINARY_DIR})
set(HGAMER3D_TGZ HGamer3D-${HGamer3D_VERSION_MAJOR}.${HGamer3D_VERSION_MINOR}.${HGamer3D_VERSION_PATCH}.tar.gz)

#
# files needed
#

set(SOURCE_FILES 
		LICENSE 
		Setup.hs
		RotatingCube.hs
		Materials.hs		
		engineShutdown.hs
)

set(TARGET_FILES)
foreach(file ${SOURCE_FILES})
	add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${file}
		COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/${file} ${CMAKE_CURRENT_BINARY_DIR}/${file}
		DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${file}
		)
	set(TARGET_FILES ${TARGET_FILES} ${CMAKE_CURRENT_BINARY_DIR}/${file})
endforeach()

# build sandbox for samples, depends on tgz
add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/cabal.sandbox.config
				COMMAND cabal sandbox init
				COMMAND cabal install ${CMAKE_BINARY_DIR}/Output/${HGAMER3D_TGZ} --extra-lib-dirs=${CMAKE_BINARY_DIR}/Output
				DEPENDS ${CMAKE_BINARY_DIR}/Output/${HGAMER3D_TGZ} LICENSE Setup.hs HGamer3D-SamplePackage.cabal
				)

add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/.cabal-sandbox/add-source-timestamps
				COMMAND cabal install HGamer3D-SamplePackage.cabal
				COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_BINARY_DIR}/.cabal-sandbox/bin ${CMAKE_BINARY_DIR}/Output
				DEPENDS  ${SOURCE_FILES}
		   )

add_custom_target(tests DEPENDS output_dll output_tgz ${CMAKE_CURRENT_BINARY_DIR}/cabal.sandbox.config ${TARGET_FILES} ${CMAKE_CURRENT_BINARY_DIR}/.cabal-sandbox/add-source-timestamps)



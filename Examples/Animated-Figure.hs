-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.althainz.de/HGamer3D.html
--
-- (c) 2011 Peter Althainz
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.


-- Animated-Figure.hs

module Main where

import HGamer3D.APIs.One

stepFunc es vs time (animationBase, animationTop)  = do
	time <- getTime es 
	setAnimationTime animationBase time
	setAnimationTime animationTop time
	return (True, (animationBase, animationTop))

main = do

	-- initialize
	(es, vs)  <- initializeHG3D "HGamer3D Example - Animated Figure" "OctreeSceneManager"
	
	-- camera position
	let pos = Vec3 0.0 0.0 (80.0)
	setCameraPos vs pos
	let at = Vec3 0.0 0.0 (-300.0)
	setCameraLookAt vs at
	
	-- load sinbad character, first create template, then mesh
	let meshTemplate = MeshResource "sinbad.mesh"
	sinbad <- createMesh es meshTemplate
	setScale sinbad (Vec3 4.0 4.0 4.0)

	-- get and enable animation
	animationBase <- getAnimation sinbad "IdleBase"
	startAnimation animationBase
	animationTop <- getAnimation sinbad "IdleTop"
	startAnimation animationTop
	
	-- define light
	let col = Colour 0.5 0.5 0.5 1.0
	light <- createLight es "MainLight" col (Vec3 20.0 80.0 50.0)
	createAmbientLight es col

	time <- getTime es
	renderLoop es vs time (animationBase, animationTop) stepFunc
		
	


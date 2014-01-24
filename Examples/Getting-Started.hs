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


-- Getting-Started.hs
-- example, to create a basic running HGamer3D Framework

module Main where
 
import HGamer3D.APIs.Base

-- define the objects of the gameworld, which will be handed
-- from each instance of hte render loop to the next
type Gameworld = Object3D

-- define the render step, a function which is called each step of
-- the render loop

renderStep :: TimeMS -> Gameworld -> MHGamer3D (Bool, Gameworld)
renderStep (TimeMS time) cube = do
   -- rotate 
   yaw3D cube (Rad 0.005) 
   roll3D cube (Rad 0.002)
   return (True, cube)

    
main3DProgram = do

	-- define the 3D program
	-- initialize graphics objects and start rendering loop
	
	-- add media locations
	addResourceLocationGUI "media\\gui\\layout" "Layout"
	addResourceLocationMedia "media\\materials"
	finalizeResourceLocations

	-- camera position
	cam <- getCamera
	let pos = Vec3 5.0 5.0 80.0
	positionTo3D cam pos
	let at = Vec3 0.0 0.0 (-300.0)
	cameraLookAt cam at
	
	-- define light
	let white = Colour 1.0 1.0 1.0 1.0
	setAmbientLight white
	light <- createPointlight white (Vec3 10.0 10.0 20.0)

	-- GUI Code starts here, display hg3d logo
	loadGuiScheme "hg3d.scheme"
	logo <- loadGuiLayoutFromFile "hgamer3d.layout" ""
	addGuiElToDisplay logo

	-- create a shiny blue cube
	cubeBlue <- createCube
	setObjectMaterial cubeBlue (NamedMaterial "Template/Blue")
	positionTo3D cubeBlue (Vec3 0.0 0.0 0.0)
	scaleTo3D cubeBlue (Vec3 0.2 0.2 0.2)

	-- start render loop
	renderLoop 40 cubeBlue renderStep
	return ()

main = do
	-- initialize HGamer3D
	hg <- initHGamer3D "HGamer3D - Getting Started Example" 
	-- run HGamer3D routines, here main 3D program
	(l, hg) <- runMHGamer3D hg main3DProgram 
	return ()

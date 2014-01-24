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


-- Rotating-Mesh.hs
--
-- the keys "D,A,S,W,Q,E" rotate the loaded mesh

module Main where

import HGamer3D.BaseAPI
import Control.Monad.Reader
import HGamer3D.Data.HG3DClass
import qualified Data.Map as Map

-- a Rotater combines a rotation with a keystroke
data Rotater = Rotater UnitQuaternion EnumKey

createRotater vector name key = do
	let quat = rotU vector 0.01
	let rot = Rotater quat key
	return (rot)
	
doRotater (Rotater quat key) object = do
	isR <- isKeyPressed key
	if isR then do
		qNow <- orientation3D object
		orientationTo3D object (quat .*. qNow)
		return ()
		else return ()


createRotaters = do
	-- define rotation quats
	let arr = [	 ( (Vec3 0.0 1.0 0.0), "right", KeyD ),
					 ( (Vec3 0.0 (-1.0) 0.0), "left", KeyA), 
					 ( (Vec3 1.0 0.0 0.0), "down", KeyS), 
					 ( (Vec3 (-1.0) 0.0 0.0), "up", KeyW),
					 ( (Vec3 0.0 0.0 1.0), "left2", KeyQ), 
					 ( (Vec3 0.0 0.0 (-1.0)), "right2", KeyE)  ]
	 
	rotators <- sequence $ map ( \(vector, name, key) -> createRotater vector name key ) arr
	return (rotators)


stepFunc time (object, rotators) = do
	sequence $ map (\r -> doRotater r object) rotators
	return (True, (object, rotators))


initAll = do

	-- add media locations
	addResourceLocationGUI "media\\gui\\layout" "Layout"
	addResourceLocationMedia "media\\ogre\\materials"
	addResourceZipfileMedia "media\\ogre\\Sinbad.zip"
	finalizeResourceLocations
	
	-- camera position
	cam <- getCamera
	let pos = Vec3 0.0 0.0 80.0
	positionTo3D cam pos
	let at = Vec3 0.0 0.0 (-300.0)
	cameraLookAt  cam at
	
	-- load ogre
	
	ogre <- createNamedMeshObject "sinbad.mesh"
	scaleTo3D ogre (Vec3 4.0 4.0 4.0)
	
	-- define light
	let col = Colour 1.0 1.0 1.0 1.0
	light <- createPointLight col (Vec3 20.0 80.0 50.0)
	setAmbientLight col

	rotators <- createRotaters
	
	-- GUI Code starts here, display hg3d logo
	loadScheme "hg3d.scheme"
	logo <- loadWindowFromFile "hgamer3d.layout" ""
	addWindowToDisplay logo
	
	
	let eventMap = Map.fromList ([]::[(String, EventFunction (Object3D, [Rotater]) )])
	renderLoop 40 (ogre, rotators) eventMap stepFunc
	return ()
	

main = do

	hg <- initHGamer3D "HGamer3D - Rotating Mesh Example" 
	(l, hg) <- runMHGamer3D hg initAll 
	return ()
	

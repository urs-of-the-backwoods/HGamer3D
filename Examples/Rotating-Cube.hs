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


-- Rotating-Cube.hs

module Main where

import HGamer3D.BaseAPI
import HGamer3D.Data.HG3DClass
import qualified Data.Map as Map



-- renderStep a :: TimeMS -> a -> MEngine3D (Bool, a)
renderStep (TimeMS delta) cu = do
	q <- orientation3D cu
	let r = rotU (Vec3 0.0 0.5 1.0) ((fromIntegral delta)*2*pi/5000.0)
	let q' = r .*. q
	orientationTo3D cu q'
	return (True, cu)

initAll :: MHGamer3D Light
initAll = do

	-- add media locations
	addResourceLocationMedia "media\\materials"
	finalizeResourceLocations
	
	c <- getCamera
	positionTo3D c (Vec3 100.0 100.0 0.0)
	cameraLookAt c (Vec3 0.0 0.0 0.0)
	l1 <- createPointLight (Colour 1.0 1.0 1.0 1.0) (Vec3 (-100.0) 10.0 10.0)
	l2 <- createPointLight (Colour 1.0 1.0 1.0 1.0) (Vec3 100.0 10.0 10.0)
	cu <- createCube
	scaleTo3D cu (Vec3 0.5 0.5 0.5)
	setObjectMaterial cu (NamedMaterial "Examples/PlatonMaterial")
	setAmbientLight (Colour 1.0 1.0 1.0 1.0)
	positionTo3D cu (Vec3 0.0 0.0 0.0)
	
	-- GUI Code starts here, display hg3d logo
	loadScheme "hg3d.scheme"
	logo <- loadWindowFromFile "hgamer3d.layout" ""
	addWindowToDisplay logo

	let eventMap = Map.fromList ([]::[(String, EventFunction Object3D)])
	renderLoop 100 cu eventMap renderStep
	
	return (l1)

main = do 
	hg <- initHGamer3D "HGamer3D - Rotating Cube Example" 
	(l, hg) <- runMHGamer3D hg initAll 
	return ()

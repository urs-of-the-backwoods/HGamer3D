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


-- Colliding-Balls.hs

module Main where

import HGamer3D.BaseAPI
import Data.IORef
import Control.Monad.Trans


-- a ball, has a physics body, a 3d object and a velocity
data Ball = Ball {
	body :: Body,
	ob :: Object3D
}
	
-- function which generates 10 balls

createBalls :: MHGamer3D [Ball]
createBalls = do
	balls <- mapM (\n -> do
		let size = (Vec3 0.05 0.05 0.05)
		let pos = (Vec3 0.0 0.0 0.0) -- &+ ((Vec3 0.1 1.0 0.0) &* (fromIntegral n))
		let vel = (Vec3 0.0 0.0 0.0)
		b3d <- createSphere
		scaleTo3D b3d (Vec3 0.05 0.05 0.05)
		positionTo3D b3d pos
		bphys <- createSphereBody 100.0 pos unitQ 0.1
		setBodyLinearVelocity bphys vel
		setObjectMaterial b3d (NamedMaterial "Template/Blue")
		return $ Ball bphys b3d 
		) [ x | x <- [1..1]]
	return balls
	
moveBall :: Ball -> MHGamer3D ()
moveBall ball = do
	pos <- position3D $ body ball
	positionTo3D (ob ball) pos


-- renderStep a :: TimeMS -> a -> MEngine3D (Bool, a)
renderStep delta (c, balls) = do
	let (TimeMS ms) = delta
	-- move balls
	physicsSimulationStep ((fromIntegral ms)/500.0) 1000 0.01
	mapM moveBall balls
	-- rotate camera
	c <- getCamera
	rotObjectY c ((fromIntegral ms)/10000.0)
	cameraLookAt c (Vec3 0.0 0.0 0.0)
	
	return (True, (c, balls))

-- rotate around Y-Axis
rotObjectY object amount = do
	-- get rotation matrix
	let rm = rotMatrixY amount
	-- get position of object
	pos <- position3D object
	-- muliply rotation marix with position vector by left
	let newpos = rm *. pos
	-- set new position of object
	positionTo3D object newpos



initAll :: MHGamer3D ()
initAll = do

	-- add media locations
	addResourceLocationMedia "media\\materials"
	finalizeResourceLocations
	
	c <- getCamera
	positionTo3D c (Vec3 80.0 70.0 10.0)
	cameraLookAt c (Vec3 0.0 0.0 0.0)
	let white = (Colour 1.0 1.0 1.0 1.0)
	l1 <- createPointlight white (Vec3 (-10.0) 50.0 80.0)
	l2 <- createPointlight white (Vec3 10.0 50.0 80.0)
	setAmbientLight white
	
	balls <- createBalls

	-- bounding cube
	let col = Colour 1.0 1.0 1.0 1.0
	blueT <- createColouredCubeMesh (NamedMaterial "YWing/Blue50") col
	redT <- createColouredCubeMesh (NamedMaterial "YWing/Red50") col
	yellowT <- createColouredCubeMesh (NamedMaterial "YWing/Yellow50") col

	mapM ( \(x, y, z, t) -> do
		let pos = (Vec3 x y z)
		cu <- createObject3DFromMesh t
		createStaticPlaneBody 0.0 pos unitQ pos
		positionTo3D cu pos
		let vs = (Vec3 sx sy sz) where
									f x = if x == 0.0 then 32.0 else 1.0
									sx = f x
									sy = f y
									sz = f z
		scaleTo3D cu vs
		return ()
		) [
			((-32.0), 0.0, 0.0, redT),
			(0.0, (-32.0), 0.0, blueT),
			(0.0, 0.0, (-32.0), yellowT)
			]

	setAmbientLight (Colour 1.0 1.0 1.0 1.0)
	
	-- GUI Code starts here, display hg3d logo
	loadGuiScheme "hg3d.scheme"
	logo <- loadGuiLayoutFromFile "hgamer3d.layout" ""
	addGuiElToDisplay logo

	renderLoop 50 (c, balls) renderStep
	
	return ()

main = do 
	hg <- initHGamer3D "HGamer3D - Jumping Balls Example" 
	(l, hg) <- runMHGamer3D hg initAll 
	return ()

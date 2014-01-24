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


-- Jumping-Balls.hs

module Main where

import HGamer3D.APIs.Base
import Data.IORef
import Control.Monad.Trans


-- a ball, has a position, a 3d object and a velocity
data Ball = Ball {
	pos :: Vec3,
	ob :: Object3D,
	vel :: Vec3 }
	
-- function which generates 100 balls

createBalls :: MHGamer3D [Ball]
createBalls = do
	balls <- mapM (\n -> do
		b3d <- createSphere	
		setObjectMaterial b3d (NamedMaterial "Template/Blue")
		scaleTo3D b3d (Vec3 0.05 0.05 0.05)
		let pos = ((Vec3 0.0 0.0 0.0) &+ ((Vec3 0.1 1.0 0.0) &* (fromIntegral n)))
		positionTo3D b3d pos
		return $ Ball pos b3d (Vec3 (0.2 * (cos ((fromIntegral n)/30.0))) 0.2 (0.2 * (sin ((fromIntegral n)/30.0))))
		) [ x | x <- [1..200]]
	return balls
	
timestepBall :: Ball -> TimeMS -> Ball
timestepBall ball (TimeMS ms) = Ball ( (pos ball) &+ ( (vel ball) &* ( (fromIntegral ms) / 10.0) ) ) (ob ball) (vel ball)

moveBall :: Ball -> MHGamer3D ()
moveBall ball = do
	positionTo3D (ob ball) (pos ball)

map2Vec :: (Float -> Float -> Float) -> Vec3 -> Vec3 -> Vec3
map2Vec f (Vec3 inx iny inz) (Vec3 in2x in2y in2z) = (Vec3 outx outy outz) where
	outx = f inx in2x
	outy = f iny in2y
	outz = f inz in2z
	
bounceBall :: Ball -> Ball
bounceBall (Ball pos o vel) = Ball pos o velout where
	func p v = if p > 30.0 then (-1.0) * (abs v) else (if p < (-30.0) then (abs v) else v)
	velout = map2Vec func pos vel

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



-- renderStep a :: TimeMS -> a -> MEngine3D (Bool, a)
renderStep delta (c, balls) = do
	-- timestepBall
	let balls2 = map (\ball -> timestepBall ball delta) balls
	let balls3 = map bounceBall balls2
	-- move balls
	mapM moveBall balls3
	-- rotate camera
	let (TimeMS ms) = delta
	rotObjectY c ((fromIntegral ms)/10000.0)
	cameraLookAt c (Vec3 0.0 0.0 0.0)
	
	return (True, (c, balls3))


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
		cu <- createObject3DFromMesh t
		positionTo3D cu (Vec3 x y z)
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

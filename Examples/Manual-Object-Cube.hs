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

-- Manual-Object-Cube.hs
--
-- the keys "D,A,S,W,Q,E" rotate the cube

module Main where

import HGamer3D.APIs.One
import HGamer3D.APIs.Two.InputSystem -- used for key input only

import HGamer3D.Bindings.Ogre.ClassManualObject as ManualObject
import HGamer3D.Bindings.Ogre.ClassSceneManager as SceneManager
import HGamer3D.Bindings.Ogre.ClassSceneNode as SceneNode
import HGamer3D.Bindings.Ogre.ClassEntity as Entity
import HGamer3D.Data.HG3DClass
import HGamer3D.Bindings.Ogre.EnumRenderOperationOperationType

-- a Rotater combines a rotation with a keystroke
data Rotater = Rotater UnitQuaternion EnumKey

createRotater vector name key = do
	let quat = rotU vector 0.001
	let rot = Rotater quat key
	return (rot)
	
doRotater (Rotater quat key) object = do
	isR <- isKeyPressed key
	if isR then do
		qNow <- getOrientation object
		setOrientation object (fromNormal (quat .*. (mkNormal qNow)))
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

stepFunc es vs time (object, rotators) = do
	sequence $ map (\r -> doRotater r object) rotators
	return (True, (object, rotators))

--
-- thanks to MadMarx (this is equivalent to Tutorial 4 code)
-- http://www.ogre3d.org/tikiwiki/MadMarx+Tutorial+4&structure=Tutorials
--

-- object functions
createCubeObject :: EngineSystem -> IO (MeshTemplate)
createCubeObject es = do
	mo <- SceneManager.createManualObject (esSceneManager es) "cube1"
	-- set Dynamic to false
	ManualObject.setDynamic mo False
	
	-- basic parameters
	let lsize = 5.0
	let cp = 1.0 * lsize
	let cm = -1.0 * lsize
	
	ManualObject.begin mo "BaseWhiteNoLighting" OT_TRIANGLE_LIST "General"
	
	sequence $ map (\(x, y, z, c) -> ManualObject.position2 mo x y z >> ManualObject.colour mo c) [
		(cm, cp, cm, (Colour 0.0 1.0 0.0 1.0) ),
		(cp, cp, cm, (Colour 1.0 1.0 0.0 1.0) ),
		(cp, cm, cm, (Colour 1.0 0.0 0.0 1.0) ),
		(cm, cm, cm, (Colour 0.0 0.0 0.0 1.0) ),
		
		(cm, cp, cp, (Colour 0.0 1.0 1.0 1.0) ),
		(cp, cp, cp, (Colour 1.0 1.0 1.0 1.0) ),
		(cp, cm, cp, (Colour 1.0 0.0 1.0 1.0) ),
		(cm, cm, cp, (Colour 0.0 0.0 1.0 1.0) )   ]
	
	sequence $ map (\(x,y,z) -> ManualObject.triangle mo x y z) [
		(0, 1, 2),
		(2, 3, 0),
		(4, 6, 5),
		(6, 4, 7),
		
		(0, 4, 5),
		(5, 1, 0),
		(2, 6, 7),
		(7, 3, 2),
		
		(0, 7, 4),
		(7, 0, 3),
		(1, 5, 6),
		(6, 2, 1) ]
	
	ManualObject.end mo
	mt <- meshTemplateFromManual es "cube1" mo 
	return (mt)


main = do

	-- initialize
	(es, vs) <- initializeHG3D "HGamer3D Example - Manual Object Cube"  "OctreeSceneManager"
	
	-- camera position
	let pos = Vec3 0.0 0.0 80.0
	setCameraPos vs pos
	let at = Vec3 0.0 0.0 (-300.0)
	setCameraLookAt vs at
	
	-- load cube
	cubeT <- createCubeObject es
	cube <- createMesh es cubeT
	
	-- define light
	let col = Colour 0.5 0.5 0.5 1.0
	light <- HGamer3D.APIs.One.createLight es "MainLight" col (Vec3 20.0 80.0 50.0)
	
	
	rotators <- createRotaters
	time <- getTime es
	renderLoop es vs time (cube, rotators) stepFunc
	
		
	


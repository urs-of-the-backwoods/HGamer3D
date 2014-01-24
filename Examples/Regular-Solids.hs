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

-- Regular-Solids.hs
--
-- the keys "D,A,S,W,Q,E" rotate the solids

module Main where

import Data.Vect

import HGamer3D.APIs.One
import HGamer3D.APIs.Two.InputSystem -- used for key input only

import HGamer3D.Bindings.Ogre.ClassManualObject as ManualObject
import HGamer3D.Bindings.Ogre.ClassSceneManager as SceneManager
import HGamer3D.Bindings.Ogre.ClassSceneNode as SceneNode
import HGamer3D.Bindings.Ogre.ClassEntity as Entity
import HGamer3D.Data.HG3DClass
import HGamer3D.Bindings.Ogre.EnumRenderOperationOperationType
import HGamer3D.Bindings.Ogre.ClassViewport as Viewport

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

vminus = Vec3 (-1.0) (-1.0) (-1.0)

getNormOfFace vertices (a, b, c) = normalize cross
							where 
								v1 = vertices !! a
								v2 = vertices !! b
								v3 = vertices !! c 
								cross = (v2 &- v1) `crossprod` (v3 &- v1)
								

isIn (a, b, c) n = if n == a then True else 
					if n == b then True else
					  if n == c then True else
						False


createPlatonObject :: EngineSystem -> String -> String -> Colour -> [Vec3] -> [(Int, Int, Int)] -> Int -> IO (MeshTemplate)
createPlatonObject es name material colour vertices faces iColor = do

	mo <- SceneManager.createManualObject (esSceneManager es) name
	-- set Dynamic to false
	ManualObject.setDynamic mo False

	let normsOfFaces = map (getNormOfFace vertices) faces
	
	let vzero = Vec3 0.0 0.0 0.0 
	
	let normsOfVertices = map vnorm allVerticesIndexes
		where	
			allVerticesIndexes = [ i | i <- [0..(length vertices)]] 
			vnorm = (\i -> normalize $ foldr (&+) vzero (map (\face -> if isIn face i then normsOfFaces !! i else vzero) faces))

	sequence $ map (\((x,y,z), norm) -> do
		ManualObject.begin mo material OT_TRIANGLE_LIST "General"
		ManualObject.position mo (vertices !! x)
		ManualObject.normal mo norm
		ManualObject.position mo (vertices !! y)
		ManualObject.position mo (vertices !! z)
		ManualObject.triangle mo 0 1 2
		ManualObject.end mo) (zip faces normsOfFaces)
	
	
	mt <- meshTemplateFromManual es (name ++ "mesh") mo 
	return (mt)


createIkosaederObject :: EngineSystem -> String -> String -> Colour -> IO (MeshTemplate)
createIkosaederObject es name material colour = do

	let x = 0.525731112119133606
	let z = 0.850650808352039932
	
	let vertices = [
		( Vec3 (-x) 0.0 z),
		( Vec3 x 0.0 z),
		( Vec3 (-x) 0.0 (-z)),
		( Vec3 x 0.0 (-z)),
		( Vec3 0.0 z x), 
		( Vec3 0.0 z (-x)), 
		( Vec3 0.0 (-z) x), 
		( Vec3 0.0 (-z) (-x)),
		( Vec3 z x 0.0),
		( Vec3 (-z) x 0.0), 
		( Vec3 z (-x) 0.0),
		( Vec3 (-z) (-x) 0.0)
		]
		
	let faces = [

		(0,4,1), (0,9,4), (9,5,4), (4,5,8), (4,8,1), 
		(8,10,1), (8,3,10), (5,3,8), (5,2,3), (2,7,3),
		(7,10,3), (7,6,10), (7,11,6), (11,0,6), (0,1,6),
		(6,1,10), (9,0,11), (9,11,2), (9,2,5), (7,2,11)
		
		]
		
	po <- createPlatonObject es name material colour vertices faces 0
	return (po)
	

	
createDodekaederObject es name material colour = do

	let s = 0.618034
	let t = 1.618034

	
	let vertices = [
	
		(Vec3 1.0 1.0 1.0),  
		(Vec3 1.0 1.0 (-1.0)),  
		(Vec3 1.0 (-1.0) 1.0), 
		(Vec3 1.0 (-1.0) (-1.0)),   
		(Vec3 (-1.0) 1.0 1.0),  
		(Vec3 (-1.0) 1.0 (-1.0)),  
		(Vec3 (-1.0) (-1.0) 1.0),   
		(Vec3 (-1.0) (-1.0) (-1.0)),  
		(Vec3 s t 0.0),
		(Vec3 (-s) t 0.0),   
		(Vec3 s (-t) 0.0),
		(Vec3 (-s) (-t) 0.0),  
		(Vec3 t 0.0 s), 
		(Vec3 t 0.0 (-s)),
		(Vec3 (-t) 0.0 s), 
		(Vec3 (-t) 0.0 (-s)),  
		(Vec3 0.0 s t),
		(Vec3 0.0 (-s) t),  
		(Vec3 0.0 s (-t)),  
		(Vec3 0.0 (-s) (-t))

		]
		
	let faces = [
	
--		(1,8,0,12,13), (4, 9, 5, 15, 14), 
--		(2, 10, 3, 13, 12), (7, 11, 6, 14, 15), 
--		(2, 12, 0, 16, 17), (1, 13, 3, 19, 18),
--		(4, 14, 6, 17, 16), (7, 15, 5, 18, 19),
--		(4, 16, 0, 8, 9), (2, 17, 6, 11, 10),
--		(1, 18, 5, 9, 8), (7, 19, 3, 10, 11)
		
		(1,8,0,12,13), (4, 9, 5, 15, 14), 
		(2, 10, 3, 13, 12), (7, 11, 6, 14, 15), 
		(2, 12, 0, 16, 17), (1, 13, 3, 19, 18),
		(4, 14, 6, 17, 16), (7, 15, 5, 18, 19),
		(4, 16, 0, 8, 9), (2, 17, 6, 11, 10),
		(1, 18, 5, 9, 8), (7, 19, 3, 10, 11)
		
		]
		
	let faces2 = foldl (++) [] $ map ( \(a, b, c, d, e) ->  [ (a, b, e), (b, d, e), (c, d, b) ]  ) faces

	po <- createPlatonObject es name material colour vertices faces2 0
	return (po)




main = do

	-- initialize
	(es, vs) <- initializeHG3D "HGamer3D Example - Regular Solids"  "OctreeSceneManager"
	let bgColour = Colour 0.3 0.3 0.5 1.0
	Viewport.setBackgroundColour (vsViewport vs) bgColour

	-- camera position
	let pos = Vec3 0.0 0.0 80.0
	setCameraPos vs pos
	let at = Vec3 0.0 0.0 (-300.0)
	setCameraLookAt vs at
	
	-- load ikosaeder
	ikoT <- createIkosaederObject es "iko" "Examples/PlatonMaterial" (Colour 1.0 0.0 1.0 1.0)
	dodeT <- createDodekaederObject es "dode" "Examples/PlatonMaterial" (Colour 1.0 0.0 1.0 1.0)

	iko <- createMesh es ikoT
	scale iko (Vec3 20.0 20.0 20.0)
	translate iko (Vec3 20.0 0.0 0.0) TS_LOCAL
	
	dode <- createMesh es dodeT
	scale dode (Vec3 10.0 10.0 10.0)
	translate dode (Vec3 (-20.0) 0.0 0.0) TS_LOCAL
	
	items <- combineGraphicsObjects es [iko, dode] 
	
	-- define light
	let col = Colour 1.0 1.0 1.0 1.0
	light <- HGamer3D.APIs.One.createLight es "MainLight" col (Vec3 10.0 20.0 30.0)
	createAmbientLight es col
	
	
	rotators <- createRotaters
	time <- getTime es
	renderLoop es vs time (items, rotators) stepFunc
	
		
	


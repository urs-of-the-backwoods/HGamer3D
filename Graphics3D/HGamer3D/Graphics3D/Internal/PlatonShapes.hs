{-# LANGUAGE FlexibleContexts #-}

-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011-2013 Peter Althainz
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

-- Graphics3D/Internal/PlatonShapes.hs

-- | Create platonic shapes, ikosaeder and dodekaeder, implementation module. Public API is in HGamer3D.Graphics3D.
module HGamer3D.Graphics3D.Internal.PlatonShapes (

	ikosaeder,
	dodekaeder
) 

where


import GHC.Ptr

import HGamer3D.Data
import HGamer3D.Data.HG3DClass
import HGamer3D.Util

import HGamer3D.Bindings.Ogre.ClassPtr
import HGamer3D.Bindings.Ogre.Utils

import HGamer3D.Bindings.Ogre.StructColour
import HGamer3D.Bindings.Ogre.StructSharedPtr
import HGamer3D.Bindings.Ogre.StructHG3DClass

import HGamer3D.Bindings.Ogre.EnumSceneType
import HGamer3D.Bindings.Ogre.EnumNodeTransformSpace
import HGamer3D.Bindings.Ogre.EnumLightType
import HGamer3D.Bindings.Ogre.EnumRenderOperationOperationType
import HGamer3D.Bindings.Ogre.EnumSceneManagerPrefabType

--import HGamer3D.Bindings.Ogre.ClassCamera as Camera
--import HGamer3D.Bindings.Ogre.ClassRoot as Root
import HGamer3D.Bindings.Ogre.ClassLight as Light
import HGamer3D.Bindings.Ogre.ClassNode as Node
import HGamer3D.Bindings.Ogre.ClassSceneManager as SceneManager
import HGamer3D.Bindings.Ogre.ClassSceneNode as SceneNode
import HGamer3D.Bindings.Ogre.ClassRenderTarget as RenderTarget
import HGamer3D.Bindings.Ogre.ClassRenderWindow as RenderWindow
import HGamer3D.Bindings.Ogre.ClassResourceGroupManager as ResourceGroupManager
import HGamer3D.Bindings.Ogre.ClassTextureManager as TextureManager
import HGamer3D.Bindings.Ogre.ClassControllerManager as ControllerManager
import HGamer3D.Bindings.Ogre.ClassViewport as Viewport
import HGamer3D.Bindings.Ogre.ClassFrustum as Frustum
import HGamer3D.Bindings.Ogre.ClassAnimationState as AnimationState
import HGamer3D.Bindings.Ogre.ClassEntity as Entity
import HGamer3D.Bindings.Ogre.ClassControllerManager as ControllerManager
import HGamer3D.Bindings.Ogre.ClassWindowEventUtilities as WindowEventUtilities
import HGamer3D.Bindings.Ogre.ClassManualObject as ManualObject

import HGamer3D.Graphics3D.Internal.Base

import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.State.Class

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


_createPlatonObject :: Graphics3DSystem -> Colour -> [Vec3] -> [(Int, Int, Int)] -> Int -> IO HG3DClass
_createPlatonObject g3ds colour vertices faces iColor = do

	let (SceneManager scm) = (g3dsSceneManager g3ds)
	uid <- nextUniqueName (g3dsUniqueName g3ds)

	mo <- SceneManager.createManualObject scm uid
	-- set Dynamic to false
	ManualObject.setDynamic mo False

	let normsOfFaces = map (getNormOfFace vertices) faces
	
	let vzero = Vec3 0.0 0.0 0.0 
	
	let normsOfVertices = map vnorm allVerticesIndexes
		where	
			allVerticesIndexes = [ i | i <- [0..(length vertices)]] 
			vnorm = (\i -> normalize $ foldr (&+) vzero (map (\face -> if isIn face i then normsOfFaces !! i else vzero) faces))

	sequence $ map (\((x,y,z), norm) -> do
		ManualObject.begin mo "BaseWhiteNoLightning" OT_TRIANGLE_LIST "General"
		ManualObject.position mo (vertices !! x)
		ManualObject.normal mo norm
		ManualObject.position mo (vertices !! y)
		ManualObject.position mo (vertices !! z)
		ManualObject.triangle mo 0 1 2
		ManualObject.end mo) (zip faces normsOfFaces)

	return mo
	

-- | create an ikoaeder mesh - from the mesh more objects can be created
ikosaeder :: Graphics3DSystem -- ^ the Graphics3D system object, returned by initGraphics3D
                       -> Colour      -- ^ colour of the ikosaeder
                       -> IO HG3DClass  -- ^ created mesh object as entity
ikosaeder g3ds colour = do
	let (SceneManager scm) = (g3dsSceneManager g3ds)
	uid <- nextUniqueName (g3dsUniqueName g3ds)

	let bodyName = "B" ++ uid
	let meshName = "M" ++ uid
	
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
		
	mo <- _createPlatonObject g3ds colour vertices faces 0
	ManualObject.convertToMesh mo meshName "General"
        entity <- SceneManager.createEntity3 scm meshName
        return $ entity
	

-- | create a dodekaeder mesh
dodekaeder :: Graphics3DSystem -- ^ the Graphics3D system object, returned by initGraphics3D
                        -> Colour -- ^ colour of the dodekaeder
                        -> IO HG3DClass -- ^ created dodekaeder mesh as entity
dodekaeder g3ds colour = do
	let (SceneManager scm) = (g3dsSceneManager g3ds)
	uid <- nextUniqueName (g3dsUniqueName g3ds)

	let bodyName = "B" ++ uid
	let meshName = "M" ++ uid
	
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

	mo <- _createPlatonObject g3ds colour vertices faces2 0
	ManualObject.convertToMesh mo meshName "General"
        entity <- SceneManager.createEntity3 scm meshName
        return $ entity

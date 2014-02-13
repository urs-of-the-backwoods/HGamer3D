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

-- Object3D.hs

-- | Creating and managing 3D objects functionality for Graphics3D module
module HGamer3D.Graphics3D.Object3D (

    -- * Mesh creation functions, standard meshes
	sphereMesh,
	cubeMesh,
        planeMesh,
	resourceMesh,
        
    -- * Mesh creation functions, special meshes    
	colouredCubeMesh,
	colouredLineMesh,
	rainbowCubeMesh,
	
    -- * clone objects from meshes    
        object3DFromMesh,
        object3DFromObjects,
        
    -- * Special movements
	yaw3D,
	roll3D,
	pitch3D
) 

where

import HGamer3D.Data
import HGamer3D.Util
  
import GHC.Ptr

import HGamer3D.Bindings.Ogre.ClassPtr
import HGamer3D.Bindings.Ogre.Utils

import HGamer3D.Bindings.Ogre.StructColour
import HGamer3D.Bindings.Ogre.StructSharedPtr

import HGamer3D.Bindings.Ogre.EnumSceneType
import HGamer3D.Bindings.Ogre.EnumNodeTransformSpace
import HGamer3D.Bindings.Ogre.EnumLightType

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
import HGamer3D.Bindings.Ogre.EnumRenderOperationOperationType
import HGamer3D.Bindings.Ogre.StructHG3DClass
import HGamer3D.Bindings.Ogre.EnumSceneManagerPrefabType

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Operation3D
import HGamer3D.Graphics3D.Types
import HGamer3D.Graphics3D.Engine

import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.State.Class

				
-- set the material of an Ogre entity, used during mesh creation
_setMaterial :: HG3DClass -- ^ 3d object
                     -> Material -- ^ material
                     -> IO ()
_setMaterial entity material = do
  case material of
    (ResourceMaterial name) -> do
	Entity.setMaterialName entity name "General"
	return ()
        

-- | creates a Sphere Mesh, from which Spheres can be cloned
sphereMesh :: Mesh -- ^ the mesh created
sphereMesh = SphereMesh
  
-- | creates a Cube Mesh, from which Cubes can be cloned
cubeMesh :: Mesh -- ^ the mesh created
cubeMesh = CubeMesh

-- | creates a Plane Mesh, from which Planes can be cloned
planeMesh :: Mesh -- ^ the mesh created
planeMesh = PlaneMesh

-- | creates a Plane Mesh, from which Planes can be cloned
resourceMesh :: String -- ^ the name of the resource
                -> Mesh -- ^ the mesh created
resourceMesh resourceName = ResourceMesh resourceName
        
-- | clones a 3D object from a mesh
object3DFromMesh :: Graphics3DSystem -- ^ the Graphics3D system object, returned by initGraphics3D
                          -> Mesh -- ^ mesh used for creation
                          -> Maybe Material -- ^ a material to be applied, if needed
                          -> IO Object3D -- ^ created 3d object
object3DFromMesh g3ds mesh mbMaterial = do
	let (SceneManager scm) = (g3dsSceneManager g3ds)
        -- create the entity from the mesh
        entity <- case mesh of
          CubeMesh -> SceneManager.createEntity6 scm PT_CUBE
          SphereMesh -> SceneManager.createEntity6 scm PT_SPHERE
          PlaneMesh -> SceneManager.createEntity6 scm PT_PLANE
          ResourceMesh resourceName -> SceneManager.createEntity3 scm resourceName
          ManualMesh meshName -> SceneManager.createEntity3 scm meshName
        -- apply material, if needed, wanted
        case mbMaterial of
          Just material -> _setMaterial entity material
          Nothing -> return ()
	-- now create node and attach entity to it
	rootNode <- SceneManager.getRootSceneNode scm
	let vzero = Vec3 0.0 0.0 0.0
	let qident = Q (Vec4 1.0 0.0 0.0 0.0)
	node <- SceneNode.createChildSceneNode rootNode vzero qident
	SceneNode.attachObject node entity
	-- return object
	return (SingleObject3D mesh node)
		
_getNode :: Object3D -> HG3DClass
_getNode (SingleObject3D entity node) = node
_getNode (CombinedObject3D objects node) = node

-- | combine 3D objects to a new object
object3DFromObjects :: Graphics3DSystem -- ^ the Graphics3D system object, returned by initGraphics3D
                       ->[Object3D] -- ^ A list of objects, to be grouped
                       -> IO Object3D -- ^ The return value is a new 3d object.
object3DFromObjects g3ds listObjects = do
	let (SceneManager scm) = (g3dsSceneManager g3ds)
	rootNode <- SceneManager.getRootSceneNode scm
	let vzero = Vec3 0.0 0.0 0.0
	let qident = Q (Vec4 1.0 0.0 0.0 0.0)
	node <- SceneNode.createChildSceneNode rootNode vzero qident
	sequence_ (map ( \object -> do
                            let objectnode = _getNode object
                            parent <- Node.getParent objectnode
                            Node.removeChild2 parent objectnode
                            Node.addChild node objectnode
                            return ()   )                     listObjects)
	return ( CombinedObject3D listObjects node)
		
-- | Creates a line with a colour from start and end point (mesh as object template)
colouredLineMesh :: Graphics3DSystem -- ^ the Graphics3D system object, returned by initGraphics3D
                          -> Vec3 -- ^ start point
                          -> Vec3 -- ^ end point 
                          -> Colour -- ^ colour
                          -> IO Mesh -- ^ line mesh
colouredLineMesh g3ds vStart vEnd colour = do
	uid <- nextUniqueName (g3dsUniqueName g3ds)
	let (SceneManager scm) = (g3dsSceneManager g3ds)
	let lineName = "L" ++ uid
	let meshName = "M" ++ uid
        
	mo <- SceneManager.createManualObject scm lineName
	ManualObject.begin mo "BaseWhiteNoLighting" OT_LINE_LIST "General"
	ManualObject.position mo vStart
	ManualObject.colour mo colour
	ManualObject.position mo vEnd
	ManualObject.colour mo colour
	ManualObject.end mo
	ManualObject.convertToMesh mo meshName "General"
	return $ ManualMesh meshName

-- |Creates a coloured cube mesh
colouredCubeMesh :: Graphics3DSystem -- ^ the Graphics3D system object, returned by initGraphics3D
                          -> Colour -- ^ colour
                          -> IO Mesh -- ^ cube mesh
colouredCubeMesh g3ds colour = do
	uid <- nextUniqueName (g3dsUniqueName g3ds)
	let (SceneManager scm) = (g3dsSceneManager g3ds)
	let cubeName = "C" ++ uid
	let meshName = "M" ++ uid
        
	mo <- SceneManager.createManualObject scm cubeName
	
	-- basic parameters
	let lsize = 1.0
	let cp = 1.0 * lsize
	let cm = -1.0 * lsize
	
	ManualObject.begin mo "BaseWhiteNoLighting" OT_TRIANGLE_LIST "General"
	
	ManualObject.position2 mo cm cp cm   -- a vertex
	ManualObject.colour mo colour
	ManualObject.position2 mo cp cp cm   -- a vertex
	ManualObject.colour mo colour
	ManualObject.position2 mo cp cm cm   -- a vertex
	ManualObject.colour mo colour
	ManualObject.position2 mo cm cm cm   -- a vertex
	ManualObject.colour mo colour
	
	ManualObject.position2 mo cm cp cp   -- a vertex
	ManualObject.colour mo colour
	ManualObject.position2 mo cp cp cp   -- a vertex
	ManualObject.colour mo colour
	ManualObject.position2 mo cp cm cp   -- a vertex
	ManualObject.colour mo colour
	ManualObject.position2 mo cm cm cp   -- a vertex
	ManualObject.colour mo colour
	
	ManualObject.triangle mo 0 1 2
	ManualObject.triangle mo 2 3 0
	ManualObject.triangle mo 4 6 5
	ManualObject.triangle mo 6 4 7
	
	ManualObject.triangle mo 0 4 5 
	ManualObject.triangle mo 5 1 0
	ManualObject.triangle mo 2 6 7
	ManualObject.triangle mo 7 3 2

	ManualObject.triangle mo 0 7 4
	ManualObject.triangle mo 7 0 3
	ManualObject.triangle mo 1 5 6
	ManualObject.triangle mo 6 2 1
	
	ManualObject.end mo
	
	ManualObject.convertToMesh mo meshName "General"
	return $ ManualMesh meshName


-- | Creates a rainbow coloured cube mesh
rainbowCubeMesh :: Graphics3DSystem -- ^ the Graphics3D system object, returned by initGraphics3D
                   -> IO Mesh
rainbowCubeMesh  g3ds = do
	uid <- nextUniqueName (g3dsUniqueName g3ds)
	let (SceneManager scm) = (g3dsSceneManager g3ds)
	let cubeName = "C" ++ uid
	let meshName = "M" ++ uid
	let materialName = "BaseWhiteNoLighting"
	
	mo <- SceneManager.createManualObject scm cubeName
	
	-- basic parameters
	let lsize = 1.0
	let cp = 1.0 * lsize
	let cm = -1.0 * lsize

	ManualObject.begin mo "BaseWhiteNoLighting" OT_TRIANGLE_LIST "General"
	
	sequence $ map (\(x, y, z, c) -> liftIO (ManualObject.position2 mo x y z) >> liftIO (ManualObject.colour mo c) ) [
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
	ManualObject.convertToMesh mo meshName "General"
	return $ ManualMesh meshName
        
instance Position3D Object3D where

	position3D obj = do
		pos <- Node.getPosition (_getNode obj)
		return (pos)
		
	positionTo3D obj pos = do
		Node.setPosition  (_getNode obj) pos
		return ()
	
instance Scale3D Object3D where

	scale3D obj = do
		pos <- Node.getScale  (_getNode obj)
		return (pos)
		
	scaleTo3D obj pos = do
		Node.setScale  (_getNode obj) pos
		return ()
	
instance Orientation3D Object3D where

	orientation3D obj = do
		q <- Node.getOrientation  (_getNode obj)
		let uq = mkNormal q
		return uq
	
	orientationTo3D obj uq = do
		Node.setOrientation  (_getNode obj) (fromNormal uq)
		return ()


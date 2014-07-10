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

-- Graphics3D/Internal/Object3D.hs

-- | Creating and managing 3D objects functionality for Graphics3D module, internal implemenatation module. Public API is HGamer3D.Graphics3D.
module HGamer3D.Graphics3D.Internal.Shapes (
  
    -- * helpers to construct simple geometries
    sphere,
    cube,
    cuboid

    -- * 
{-
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
-}      
) 

where

import HGamer3D.Data
import HGamer3D.Data.HG3DClass
import HGamer3D.Util
  
import GHC.Ptr

import HGamer3D.Bindings.Ogre.ClassPtr
import HGamer3D.Bindings.Ogre.Utils

import HGamer3D.Bindings.Ogre.StructColour
import HGamer3D.Bindings.Ogre.StructSharedPtr

import HGamer3D.Bindings.Ogre.EnumSceneType
import HGamer3D.Bindings.Ogre.EnumNodeTransformSpace
import HGamer3D.Bindings.Ogre.EnumLightType

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
import HGamer3D.Bindings.Ogre.ClassHG3DUtilities as HG3DUtils

import HGamer3D.Bindings.Ogre.ClassManualObject as ManualObject
import HGamer3D.Bindings.Ogre.EnumRenderOperationOperationType
import HGamer3D.Bindings.Ogre.StructHG3DClass
import HGamer3D.Bindings.Ogre.EnumSceneManagerPrefabType

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Transform3D
import HGamer3D.Graphics3D.Internal.Base

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Class

import HGamer3D.Graphics3D.Schema.Material
import HGamer3D.Graphics3D.Schema.Geometry
import HGamer3D.Graphics3D.Schema.Figure

import HGamer3D.Graphics3D.Internal.PlatonShapes


{- -------------------------------------------------------------------------------
   EngineItem for Geometry
   ------------------------------------------------------------------------------- -}

-- helper functions

_createGeometry :: Graphics3DSystem -> Geometry -> IO OEntity
_createGeometry g3ds geo = do
  let (SceneManager scm) = (g3dsSceneManager g3ds)
  entity <- case geo of
    ResourceGeometry s -> SceneManager.createEntity3 scm s
    Cube -> SceneManager.createEntity6 scm PT_CUBE
    Sphere -> SceneManager.createEntity6 scm PT_SPHERE
    Ikosaeder -> ikosaeder g3ds white
    Dodekaeder -> dodekaeder g3ds white
    Plane -> SceneManager.createEntity6 scm PT_PLANE
    _ -> error "HGamer3D.Graphics3D.Internal.Shapes._createMesh: Geo not implemented"
  return $ OE entity

_getRootNode :: Graphics3DSystem -> IO ONode
_getRootNode g3ds = do
  let (SceneManager scm) = (g3dsSceneManager g3ds)
  rootNode <- SceneManager.getRootSceneNode scm
  return (ON rootNode)

_createSubNode :: ONode -> IO ONode
_createSubNode (ON parent) = SceneNode.createChildSceneNode parent zeroVec3 unitQ >>= (return . ON)

_buildTV :: OEntity -> IO ()
_buildTV (OE entity) = HG3DUtils.buildTangentVectors entity

_addEntityToNode :: Graphics3DSystem -> ONode -> OEntity -> IO ()
_addEntityToNode g3ds(ON node) (OE meshEntity) = SceneNode.attachObject node meshEntity 

_exchangeEntityInNode :: Graphics3DSystem -> ONode -> OEntity -> OEntity -> IO ()
_exchangeEntityInNode g3ds (ON meshNode) (OE oldMeshEntity) (OE newMeshEntity) = do
  let (SceneManager scm) = (g3dsSceneManager g3ds)
  SceneNode.detachObject2 meshNode oldMeshEntity
  SceneManager.destroyEntity scm oldMeshEntity
  SceneNode.attachObject meshNode newMeshEntity

_removeEntityAndNode :: Graphics3DSystem -> ONode -> OEntity -> ONode -> IO ()
_removeEntityAndNode g3ds (ON parent) (OE meshEntity ) (ON meshNode) = do
  let (SceneManager scm) = (g3dsSceneManager g3ds)
  rootNode <- SceneManager.getRootSceneNode scm
  SceneNode.detachObject2 meshNode meshEntity
  SceneManager.destroyEntity scm meshEntity
  Node.removeChild2 parent meshNode
  SceneManager.destroySceneNode2 scm meshNode

-- instance definition

instance Engine3DItem Geometry where
  
  object3D g3ds geo = do
    meshEntity <- _createGeometry g3ds geo
    meshNode <- (_getRootNode g3ds >>= _createSubNode)
    _addEntityToNode g3ds meshNode meshEntity
    _buildTV meshEntity
    return (Object3D (EDEntityNode meshEntity meshNode) geo)

  update3D g3ds (Object3D (EDEntityNode meshEntity meshNode) oldGeo) geo = do
    let (ON node) = meshNode
    meshEntity' <- if oldGeo /= geo
                   then do
                     newMeshEntity <- _createGeometry g3ds geo
                     _exchangeEntityInNode g3ds meshNode meshEntity newMeshEntity
                     return newMeshEntity
                   else
                     return meshEntity
    return (Object3D (EDEntityNode meshEntity' meshNode) geo)

  remove3D g3ds (Object3D (EDEntityNode meshEntity meshNode) geo) = do
    rootNode <- _getRootNode g3ds
    _removeEntityAndNode g3ds rootNode meshEntity meshNode



{- -------------------------------------------------------------------------------
   EngineItem for Figure
   ------------------------------------------------------------------------------- -}

_createResourceFigure :: Graphics3DSystem -> String -> IO OEntity
_createResourceFigure g3ds name = do
  let (SceneManager scm) = (g3dsSceneManager g3ds)
  -- create the entity from the mesh
  entity <- SceneManager.createEntity3 scm name
  return $ OE entity

_setNodeP :: EngineData -> Position -> IO ()
_setNodeP edata pos = do
  let (ON node) = _getNode edata
  Node.setPosition node pos

_setNodeO :: EngineData -> Orientation -> IO ()
_setNodeO edata ori = do
  let (ON node) = _getNode edata
  Node.setOrientation node (fromNormal ori)

_setNodeS :: EngineData -> Size -> IO ()
_setNodeS edata size = do
  let (ON node) = _getNode edata
  Node.setScale node size

_setNodePOS :: EngineData -> Position -> Orientation -> Size -> IO ()
_setNodePOS edata pos ori size = do
  let (ON node) = _getNode edata
  Node.setPosition node pos
  Node.setOrientation node (fromNormal ori)
  Node.setScale node size
  
_setMaterial :: OEntity -> Material -> IO ()
_setMaterial (OE entity) material = do
  case material of
    (ResourceMaterial name) -> do
	Entity.setMaterialName entity name "General"
    _ -> error "HGamer3D.Graphics3D.Internal.Shapes._setMaterial: Material not implemented"

_createFigure :: Graphics3DSystem -> ONode -> Figure -> IO (EngineData)
_createFigure g3ds parent fig = do
  node <- _createSubNode parent
  case fig of
    SimpleFigure geo mat -> do
      meshEntity <- _createGeometry g3ds geo
      _buildTV meshEntity
      _setMaterial meshEntity mat
      _addEntityToNode g3ds node meshEntity
      return (EDEntityNode meshEntity node)
    ResourceFigure name -> do
      meshEntity <- _createResourceFigure g3ds name
      _buildTV meshEntity
      _addEntityToNode g3ds node meshEntity
      return (EDEntityNode meshEntity node)
    CombinedFigure arrSubs -> do
      resArr <- mapM (\(pos, ori, size, fig) -> do
                         newData <- _createFigure g3ds node fig
                         _setNodePOS newData pos ori size
                         return newData) arrSubs
      return (EDNodeAndSub node resArr)

data UpdateActions = ResetMaterial
                   | ResetGeometry
                   | ResetResource
                   | CompareSub
                   | Rebuild
                   | DoNothing
                     
                     deriving (Eq, Show)


_updateFigure :: Graphics3DSystem -> ONode -> EngineData -> Figure -> Figure -> IO (EngineData)
_updateFigure g3ds parent edata oldFig newFig = do
  let node = _getNode edata

  -- check, which action is needed
  
  let action = case oldFig of
        SimpleFigure oldGeo oldMat -> case newFig of
          SimpleFigure newGeo newMat -> if newGeo == oldGeo
                                           then
                                             if newMat == oldMat
                                                then DoNothing
                                                else ResetMaterial
                                           else ResetGeometry
          ResourceFigure name -> ResetResource
          CombinedFigure _ -> Rebuild
        ResourceFigure oldName -> case newFig of
          ResourceFigure newName -> if newName == oldName
                                       then DoNothing
                                       else ResetResource
          SimpleFigure newGeo newMat -> ResetGeometry
          CombinedFigure _ -> Rebuild
        CombinedFigure oldSubs -> case newFig of
          SimpleFigure _ _ -> Rebuild
          ResourceFigure _ -> Rebuild
          CombinedFigure newSubs -> if (length oldSubs) == (length newSubs)
                                       then CompareSub
                                       else Rebuild
          
  edataNew <-
        
        -- perform the single actions, we can keep existing node and single EngineData item form
        if action == ResetMaterial || action == ResetGeometry || action == ResetResource
           then do
             let (EDEntityNode eOld nOld) = edata
             case action of
               ResetMaterial -> do
                 let (SimpleFigure geo mat) = newFig
                 _setMaterial eOld mat
                 return edata
               ResetGeometry -> do
                 let (SimpleFigure geo mat) = newFig
                 eNew <- _createGeometry g3ds geo
                 _buildTV eNew
                 _setMaterial eNew mat
                 _exchangeEntityInNode g3ds nOld eOld eNew
                 return (EDEntityNode eNew nOld)
               ResetResource -> do
                 let (ResourceFigure name) = newFig
                 eNew <- _createResourceFigure g3ds name
                 _buildTV eNew
                 _exchangeEntityInNode g3ds nOld eOld eNew
                 return (EDEntityNode eNew nOld)

           -- perform the actions, we compare two equal length subarrays
           else if action == CompareSub
             then do
               let (CombinedFigure oldSubs) = oldFig
               let (CombinedFigure newSubs) = newFig
               let (EDNodeAndSub node' edOldArr) = edata
               outArr <- mapM ( \(oldED, (posO, oriO, sizeO, oldFig), (posN, oriN, sizeN, newFig)) -> do
                                   newED <- _updateFigure g3ds node' oldED oldFig newFig
                                   if posO /= posN then _setNodeP newED posN else return ()
                                   if oriO /= oriN then _setNodeO newED oriN else return ()
                                   if sizeO /= sizeN then _setNodeS newED sizeN else return ()
                                   return newED
                              ) (zip3 edOldArr oldSubs newSubs)
               return (EDNodeAndSub node' outArr)

             -- perform the actions, do a complete rebuild, since structure does not match
             else if action == Rebuild
                     then do
                       -- delete old structure (to be done)
                       _removeFigure g3ds parent edata
                       -- create new structure, this is the easy part
                       _createFigure g3ds parent newFig

                     -- no action needed, all the same
                     else if action == DoNothing
                          then return edata

                          else return $ error ("HGamer3D.Graphics3D.Internal.Shapes._updateFigure: build action not known " ++ (show action))
             
  return edataNew
               
_removeFigure :: Graphics3DSystem -> ONode -> EngineData -> IO ()
_removeFigure g3ds parent edata = do
  case edata of
    EDEntityNode e n -> _removeEntityAndNode g3ds parent e n
    EDNodeAndSub n subs -> (mapM (\edata' -> _removeFigure g3ds n edata') subs) >> return ()
      
instance Engine3DItem Figure where
  
  object3D g3ds fig = do
    rootNode <- _getRootNode g3ds
    edata <- _createFigure g3ds rootNode fig
    return $ Object3D edata fig

  update3D g3ds ob3d newFig = do
    rootNode <- _getRootNode g3ds
    let (Object3D oldEdata oldFig) = ob3d
    newEdata <- _updateFigure g3ds rootNode oldEdata oldFig newFig
    return $ Object3D newEdata newFig

  remove3D g3ds ob3d = do
    rootNode <- _getRootNode g3ds
    let (Object3D edata fig) = ob3d
    _removeFigure g3ds rootNode edata
    return ()


{- -------------------------------------------------------------------------------
   intelligent constructors for simple geometries
   ------------------------------------------------------------------------------- -}

sphere :: Graphics3DSystem -> Float -> Material -> IO (Object3D Figure)
sphere g3ds radius material = object3D g3ds (CombinedFigure [
                 (zeroVec3, unitU, Vec3 radius radius radius,
                  SimpleFigure Sphere material) ])

cube :: Graphics3DSystem -> Float -> Material -> IO (Object3D Figure)
cube g3ds len material = object3D g3ds (CombinedFigure [
                 (zeroVec3, unitU, Vec3 len len len,
                  SimpleFigure Cube material) ])

cuboid :: Graphics3DSystem -> Vec3 -> Material -> IO (Object3D Figure)
cuboid g3ds vec material = object3D g3ds (CombinedFigure [
                 (zeroVec3, unitU, vec, SimpleFigure Cube material) ])
                           

{-

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
                          -> Bool -- ^ flag, build tangent vectors, if set
                          -> IO Object3D -- ^ created 3d object
object3DFromMesh g3ds mesh mbMaterial buildTV = do
	let (SceneManager scm) = (g3dsSceneManager g3ds)
        -- create the entity from the mesh
        entity <- case mesh of
          CubeMesh -> SceneManager.createEntity6 scm PT_CUBE
          SphereMesh -> SceneManager.createEntity6 scm PT_SPHERE
          PlaneMesh -> SceneManager.createEntity6 scm PT_PLANE
          ResourceMesh resourceName -> SceneManager.createEntity3 scm resourceName
          ManualMesh meshName -> SceneManager.createEntity3 scm meshName
        -- create tangent vectors
        if buildTV then 
          HG3DUtils.buildTangentVectors entity
          else
            return ()
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
        
-}

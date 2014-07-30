{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide #-}

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
module HGamer3D.Graphics3D.Internal.Shapes 

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
    Ikosaeder -> ikosaederE g3ds white
    Dodekaeder -> dodekaederE g3ds white
    Plane -> SceneManager.createEntity6 scm PT_PLANE
    _ -> error "HGamer3D.Graphics3D.Internal.Shapes._createMesh: Geo not implemented"
  return $ OE entity

_buildTV :: OEntity -> IO ()
_buildTV (OE entity) = HG3DUtils.buildTangentVectors entity


{- -------------------------------------------------------------------------------
   EngineItem for Figure
   ------------------------------------------------------------------------------- -}

_createResourceFigure :: Graphics3DSystem -> String -> IO OEntity
_createResourceFigure g3ds name = do
  let (SceneManager scm) = (g3dsSceneManager g3ds)
  -- create the entity from the mesh
  entity <- SceneManager.createEntity3 scm name
  return $ OE entity

_setMaterial :: OEntity -> Material -> IO ()
_setMaterial (OE entity) material = do
  case material of
    (ResourceMaterial name) -> do
	Entity.setMaterialName entity name "General"
    _ -> error "HGamer3D.Graphics3D.Internal.Shapes._setMaterial: Material not implemented"

_setNodePOS n pos ori siz = do
  setNodePos n pos
  setNodeOri n ori
  setNodeSiz n siz
  
_createFigure :: Graphics3DSystem -> ONode -> Figure -> IO (EngineData)
_createFigure g3ds parent fig = do
  node <- _createSubNode parent
  case fig of
    SimpleFigure geo mat -> do
      meshEntity <- _createGeometry g3ds geo
      if (geo /= Dodekaeder) && (geo /= Ikosaeder) then
        _buildTV meshEntity
        else return ()
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
  let node = getNode edata

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
                                   if posO /= posN then setNodePos newED posN else return ()
                                   if oriO /= oriN then setNodeOri newED oriN else return ()
                                   if sizeO /= sizeN then setNodeSiz newED sizeN else return ()
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
      
instance Graphics3DItem Figure where
  
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
                           
dodekaeder :: Graphics3DSystem -> Float -> Material -> IO (Object3D Figure)
dodekaeder g3ds len material = object3D g3ds (CombinedFigure [
                 (zeroVec3, unitU, Vec3 len len len, SimpleFigure Dodekaeder material) ])
                           
ikosaeder :: Graphics3DSystem -> Float -> Material -> IO (Object3D Figure)
ikosaeder g3ds len material = object3D g3ds (CombinedFigure [
                 (zeroVec3, unitU, Vec3 len len len, SimpleFigure Ikosaeder material) ])
                           
instance HasPosition (Object3D Figure)  where
	position obj = Node.getPosition (getNode' obj)
	positionTo obj pos = Node.setPosition  (getNode' obj) pos
	
instance HasSize (Object3D Figure) where
	size obj = Node.getScale  (getNode' obj)
	sizeTo obj pos = Node.setScale  (getNode' obj) pos
	
instance HasOrientation (Object3D Figure) where
	orientation obj = do
		q <- Node.getOrientation  (getNode' obj)
		let uq = mkNormal q
		return uq
	orientationTo obj uq = do
		Node.setOrientation  (getNode' obj) (fromNormal uq)
		return ()
                           

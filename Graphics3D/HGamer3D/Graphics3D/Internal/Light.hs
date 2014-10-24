{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_HADDOCK hide #-}

-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011-2014 Peter Althainz
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

-- Graphics3D/Internal/Light.hs

-- | Creating and managing Light in Base API. Internal implementation module. Public API is in HGamer3D.Graphics3D.


module HGamer3D.Graphics3D.Internal.Light 
where

import Data.Maybe

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
import HGamer3D.Data.Transform3D
import HGamer3D.Graphics3D.Internal.Base

import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.State.Class

import qualified HGamer3D.Graphics3D.Schema.Light as L


-- | The light.
data Light = Light ONode OLight L.Light

instance HasNode Light where
  getNode (Light node _ _) = node

instance HasPosition Light where
	position obj = Node.getPosition (getNode' obj)
	positionTo obj pos = Node.setPosition  (getNode' obj) pos
	
instance HasOrientation Light where
	orientation obj = do
		q <- Node.getOrientation  (getNode' obj)
		let uq = mkNormal q
		return uq
	orientationTo obj uq = do
		Node.setOrientation  (getNode' obj) (fromNormal uq)
		return ()

addLight :: Graphics3DSystem -> L.Light -> IO Light
addLight g3ds schema = do
  let (SceneManager scm) = (g3dsSceneManager g3ds)
  let (L.Light diffuse@(Colour r g b _) specular@(Colour r' g' b' _) ltype) = schema
  lightName <- nextUniqueName (g3dsUniqueName g3ds)
  light <- SceneManager.createLight scm lightName
  Light.setDiffuseColour light r g b 
  Light.setSpecularColour light r' g' b' 
  case ltype of      
    L.PointLight -> Light.setType light LT_POINT
    L.DirectionalLight dir -> do
      Light.setType light LT_DIRECTIONAL
      Light.setDirection2 light dir
    L.SpotLight dir inner outer -> do
      Light.setType light LT_SPOTLIGHT
      Light.setDirection2 light dir
      Light.setSpotlightInnerAngle light (fromAngle inner)
      Light.setSpotlightOuterAngle light (fromAngle outer)
  let l = OL light
  rn <- _getRootNode g3ds
  n <- _createSubNode rn
  attachToNode l n
  return $ Light n l schema

removeLight :: Graphics3DSystem -> Light -> IO ()
removeLight g3ds (Light n@(ON node) l@(OL light) schema) = do
  let (SceneManager scm) = (g3dsSceneManager g3ds)
  detachFromNode l n
  (ON rn) <- _getRootNode g3ds
  Node.removeChild2 rn node
  SceneManager.destroyLight2 scm light 

updateLight :: Graphics3DSystem -> Light -> L.Light -> IO Light
updateLight  g3ds l schema = do
  let (SceneManager scm) = (g3dsSceneManager g3ds)
  removeLight g3ds l
  addLight g3ds schema
    
-- | Ambient light is present everywhere, this function creates it and sets the colour of it.
-- There is no light object, since movement, rotation, scaling would make no sense anyhow.
setAmbientLight :: Graphics3DSystem -- ^ the Graphics3D system object, returned by initGraphics3D
                   -> Colour -> IO () 
setAmbientLight g3ds colour = do
	let (SceneManager scm) = (g3dsSceneManager g3ds)
        SceneManager.setAmbientLight scm colour

-- | creates a point light at a specific location
pointLight :: Graphics3DSystem -- ^ the Graphics3D system object, returned by initGraphics3D
                    -> Colour -- ^ diffuse Color of the light
                    -> Colour -- ^ specular Color of the light
                    -> Vec3 -- ^ Position, where light is created
                    -> IO (Light) -- ^ The light object
pointLight g3ds diffuse specular pos = do
  let schema = L.Light diffuse specular L.PointLight
  light <- addLight g3ds schema
  positionTo light pos
  return $ light

-- | creates a spot light at a specific location
spotLight :: Graphics3DSystem -- ^ the Graphics3D system object, returned by initGraphics3D
                    -> Colour -- ^ diffuse Colour of the light
                    -> Colour -- ^ specular Colour of the light
                    -> Vec3 -- ^ Position, where light is created
                    -> Vec3 -- ^ Direction, where light points
                    -> Angle -- ^ inner Angle of cone (5..355 degrees)
                    -> Angle -- ^ outer Angle of cone (5..355 degrees)
                    -> IO Light -- ^ The light object
spotLight g3ds diffuse specular pos dir inner outer = do
  let schema = L.Light diffuse specular (L.SpotLight dir inner outer)
  light <- addLight g3ds schema
  positionTo light pos
  return $ light

-- | sets spotlight direction
spotLightSetDirection :: Graphics3DSystem -> Light -> Vec3 -> IO Light
spotLightSetDirection g3ds light dir' = do
  let (Light n l schema) = light
  let (L.Light diffuse@(Colour r g b _) specular@(Colour r' g' b' _) ltype) = schema
  case ltype of
    (L.SpotLight dir inner outer) -> updateLight g3ds light (L.Light diffuse specular (L.SpotLight dir' inner outer))
    _ -> return light

-- | creates a directional light at a specific location
directionalLight :: Graphics3DSystem -- ^ the Graphics3D system object, returned by initGraphics3D
                    -> Colour -- ^ diffuse Colour of the light
                    -> Colour -- ^ specular Colour of light
                    -> Vec3 -- ^ direction of light
                    -> IO (Light) -- ^ The light object
directionalLight g3ds diffuse specular dir = do
  let schema = L.Light diffuse specular (L.DirectionalLight dir)
  light <- addLight g3ds schema
  return $ light
  


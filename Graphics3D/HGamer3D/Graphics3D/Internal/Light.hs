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

-- Graphics3D/Internal/Light.hs

-- | Creating and managing Light in Base API. Internal implementation module. Public API is in HGamer3D.Graphics3D.


module HGamer3D.Graphics3D.Internal.Light (

    -- * Types
	Light (..),
        
    -- * create and modify light sources
	HGamer3D.Graphics3D.Internal.Light.setAmbientLight,
	pointLight,
	spotLight,
	spotLightSetDirection,
	setSpotLightAngle,
	directionalLight
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



-- | The light.
data Light = Light HG3DClass deriving (Show)

instance HasPosition Light where

	position (Light l) = do
		pos <- Light.getPosition l
		return (pos)
		
	positionTo (Light l) pos = do
		let (Vec3 x y z) = pos
		Light.setPosition l x y z
		return ()
	
spotLightSetDirection l v = Light.setDirection2 l v


-- | Ambient light is present everywhere, this function creates it and sets the colour of it.
-- There is no light object, since movement, rotation, scaling would make no sense anyhow.

setAmbientLight :: Graphics3DSystem -- ^ the Graphics3D system object, returned by initGraphics3D
                   -> Colour -> IO () 
setAmbientLight g3ds colour = do
	let (SceneManager scm) = (g3dsSceneManager g3ds)
	SceneManager.setAmbientLight scm colour
	return ()
	

-- | creates a point light at a specific location
pointLight :: Graphics3DSystem -- ^ the Graphics3D system object, returned by initGraphics3D
                    -> Colour -- ^ Color of the light
                    -> Vec3 -- ^ Position, where light is created
                    -> IO (Light) -- ^ The light object
		
pointLight g3ds (Colour r g b al) (Vec3 x y z) = do
	let (SceneManager scm) = (g3dsSceneManager g3ds)
	lightName <- nextUniqueName (g3dsUniqueName g3ds)
	
        light <- SceneManager.createLight scm lightName
	Light.setType light LT_POINT
	Light.setPosition light x y z
	Light.setDiffuseColour light r g b 
	Light.setSpecularColour light r g b 
	let eo = Light light
	return eo

-- | creates a spot light at a specific location
spotLight :: Graphics3DSystem -- ^ the Graphics3D system object, returned by initGraphics3D
                    -> Colour -- ^ Color of the light
                    -> Vec3 -- ^ Position, where light is created
                    -> IO Light -- ^ The light object
spotLight g3ds (Colour r g b al) (Vec3 x y z) = do
	let (SceneManager scm) = (g3dsSceneManager g3ds)
	lightName <- nextUniqueName (g3dsUniqueName g3ds)
	
	light <- SceneManager.createLight scm lightName
	Light.setType light LT_SPOTLIGHT
	Light.setPosition light x y z
	Light.setDiffuseColour light r g b 
	Light.setSpecularColour light r g b 
	let eo = Light light
	return eo

-- | set the angle of a spotlight
setSpotLightAngle :: Light -- ^ spotlight
					-> Angle -- ^ angle of the light cone, should be between 5 and 355 degree
					-> IO ()
setSpotLightAngle (Light light) a = do
	if (a >= (Deg 5)) && (a <= (Deg 355))
		then do
			let innerAngle = fromAngle $ a `subA` (Deg 4.5)
			let outerAngle = fromAngle $ a `addA` (Deg 4.5)
			Light.setSpotlightInnerAngle light innerAngle
			Light.setSpotlightOuterAngle light outerAngle
			return ()
		else do
			return ()
			

-- | creates a directional light at a specific location
directionalLight :: Graphics3DSystem -- ^ the Graphics3D system object, returned by initGraphics3D
                    -> Colour -- ^ Color of the light
                    -> Vec3 -- ^ Position, where light is created
                    -> IO (Light) -- ^ The light object
		
directionalLight g3ds (Colour r g b al) (Vec3 x y z) = do
	let (SceneManager scm) = (g3dsSceneManager g3ds)
	lightName <- nextUniqueName (g3dsUniqueName g3ds)
	
	light <- SceneManager.createLight scm lightName
	Light.setType light LT_DIRECTIONAL
	Light.setPosition light x y z
	Light.setDiffuseColour light r g b 
	Light.setSpecularColour light r g b 
	let eo = Light light
	return eo


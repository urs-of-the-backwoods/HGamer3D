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

-- Light.hs

-- | Creating and managing Light in Base API.


module HGamer3D.APIs.Base.Graphics3D.Light (

        -- * Types
	Light (..),
        
        -- * Create and modify light sources
	HGamer3D.APIs.Base.Graphics3D.Light.setAmbientLight,
	createPointlight,
	createSpotlight,
	setSpotlightAngle,
	createDirectionalLight
) 

where


import GHC.Ptr

import HGamer3D.Bindings.Ogre.ClassPtr
import HGamer3D.Bindings.Ogre.Utils

import HGamer3D.Data.Colour
import HGamer3D.Data.Vector
import HGamer3D.Data.Angle

import HGamer3D.Bindings.Ogre.StructColour
import HGamer3D.Bindings.Ogre.StructSharedPtr

import HGamer3D.Bindings.Ogre.EnumSceneType
import HGamer3D.Bindings.Ogre.EnumNodeTransformSpace
import HGamer3D.Bindings.Ogre.EnumLightType

import HGamer3D.Bindings.Ogre.ClassCamera as Camera
import HGamer3D.Bindings.Ogre.ClassRoot as Root
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

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State

import HGamer3D.APIs.Base.Engine.Types
import HGamer3D.APIs.Base.Engine.Engine
import HGamer3D.APIs.Base.Graphics3D.EngineHelper
import HGamer3D.APIs.Base.Graphics3D.Basic3D

-- | The light.
data Light = Light HG3DClass deriving (Show)

instance Position3D Light where

	position3D (Light l) = do
		pos <- liftIO $ Light.getPosition l
		return (pos)
		
	positionTo3D (Light l) pos = do
		let (Vec3 x y z) = pos
		liftIO $ Light.setPosition l x y z
		return ()
	
instance Direction3D Light where

	direction3D (Light l)  = do
		d <- liftIO $ Light.getDirection l
		return d
	
	directionTo3D (Light l) v = do
		liftIO $ Light.setDirection2 l v
		return ()


-- | Ambient light is present everywhere, this function creates it and sets the colour of it.
-- There is no light object, since movement, rotation, scaling would make no sense anyhow.
setAmbientLight :: Colour -> MHGamer3D () 
setAmbientLight colour = do
	rs <- ask
	let g3s = graphics3DSystem rs
	liftIO $ SceneManager.setAmbientLight (g3sSceneManager g3s) colour
	return ()
	

-- | creates a point light at a specific location
createPointlight :: Colour -- ^ Color of the light
		-> Vec3 -- ^ Position, where light is created
		-> MHGamer3D (Light) -- ^ The light object
		
createPointlight (Colour t r g b) (Vec3 x y z) = do
	rs <- ask
	let g3s = graphics3DSystem rs
	lightName <- getUniqueName "LO"
	light <- liftIO $ SceneManager.createLight (g3sSceneManager g3s) lightName
	liftIO $ Light.setType light LT_POINT
	liftIO $ Light.setPosition light x y z
	liftIO $ Light.setDiffuseColour light r g b 
	liftIO $ Light.setSpecularColour light r g b 
	let eo = Light light
	return eo

-- | creates a spot light at a specific location
createSpotlight :: Colour -- ^ Color of the light
		-> Vec3 -- ^ Position, where light is created
		-> MHGamer3D (Light) -- ^ The light object
		
createSpotlight (Colour t r g b) (Vec3 x y z) = do
	rs <- ask
	let g3s = graphics3DSystem rs
	lightName <- getUniqueName "LO"
	light <- liftIO $ SceneManager.createLight (g3sSceneManager g3s) lightName
	liftIO $ Light.setType light LT_SPOTLIGHT
	liftIO $ Light.setPosition light x y z
	liftIO $ Light.setDiffuseColour light r g b 
	liftIO $ Light.setSpecularColour light r g b 
	let eo = Light light
	return eo

-- | set the angle of a spotlight
setSpotlightAngle :: Light -- ^ spotlight
					-> Angle -- ^ angle of the light cone, should be between 5 and 355 degree
					-> MHGamer3D ()
setSpotlightAngle (Light light) a = do
	if (a >= (Deg 5)) && (a <= (Deg 355))
		then do
			let innerAngle = fromAngle $ a `subA` (Deg 4.5)
			let outerAngle = fromAngle $ a `addA` (Deg 4.5)
			liftIO $ Light.setSpotlightInnerAngle light innerAngle
			liftIO $ Light.setSpotlightOuterAngle light outerAngle
			return ()
		else do
			return ()
			

-- | creates a directional light at a specific location
createDirectionalLight :: Colour -- ^ Color of the light
		-> Vec3 -- ^ Position, where light is created
		-> MHGamer3D (Light) -- ^ The light object
		
createDirectionalLight (Colour t r g b) (Vec3 x y z) = do
	rs <- ask
	let g3s = graphics3DSystem rs
	lightName <- getUniqueName "LO"
	light <- liftIO $ SceneManager.createLight (g3sSceneManager g3s) lightName
	liftIO $ Light.setType light LT_DIRECTIONAL
	liftIO $ Light.setPosition light x y z
	liftIO $ Light.setDiffuseColour light r g b 
	liftIO $ Light.setSpecularColour light r g b 
	let eo = Light light
	return eo


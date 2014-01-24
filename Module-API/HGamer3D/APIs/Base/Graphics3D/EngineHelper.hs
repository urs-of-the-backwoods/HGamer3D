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

-- EngineHelper.hs

-- | Initialization function for 3d graphics in Base API.

module HGamer3D.APIs.Base.Graphics3D.EngineHelper (

	initGraphics3D
) 

where


import GHC.Ptr


import HGamer3D.Data.Colour
import HGamer3D.Data.Vector
import HGamer3D.Data.Angle
import HGamer3D.Data.HG3DClass

import HGamer3D.Bindings.Ogre.ClassPtr
import HGamer3D.Bindings.Ogre.Utils
import HGamer3D.Bindings.Ogre.StructColour
import HGamer3D.Bindings.Ogre.StructSharedPtr
import HGamer3D.Bindings.Ogre.EnumSceneType
import HGamer3D.Bindings.Ogre.EnumNodeTransformSpace
import HGamer3D.Bindings.Ogre.ClassCamera as Camera
import HGamer3D.Bindings.Ogre.ClassRoot as Root
import HGamer3D.Bindings.Ogre.ClassLight as Light
import HGamer3D.Bindings.Ogre.ClassNode as Node
import HGamer3D.Bindings.Ogre.ClassSceneManager as SceneManager
import HGamer3D.Bindings.Ogre.ClassSceneNode as SceneNode
import HGamer3D.Bindings.Ogre.ClassRenderTarget as RenderTarget
import HGamer3D.Bindings.Ogre.ClassRenderWindow as RenderWindow
import HGamer3D.Bindings.Ogre.ClassRenderSystem as RenderSystem
import HGamer3D.Bindings.Ogre.ClassResourceGroupManager as ResourceGroupManager
import HGamer3D.Bindings.Ogre.ClassTextureManager as TextureManager
import HGamer3D.Bindings.Ogre.ClassControllerManager as ControllerManager
import HGamer3D.Bindings.Ogre.ClassViewport as Viewport
import HGamer3D.Bindings.Ogre.ClassFrustum as Frustum
import HGamer3D.Bindings.Ogre.ClassAnimationState as AnimationState
import HGamer3D.Bindings.Ogre.ClassEntity as Entity
import HGamer3D.Bindings.Ogre.ClassControllerManager as ControllerManager
-- import HGamer3D.Bindings.Ogre.ClassWindowEventUtilities as WindowEventUtilities
import HGamer3D.Bindings.Ogre.ClassHG3DMessagePump as MessagePump
import HGamer3D.Bindings.Ogre.ClassLogManager as LogManager
import HGamer3D.Bindings.Ogre.ClassLog as Log

import HGamer3D.Bindings.Ogre.ClassManualObject as ManualObject
import HGamer3D.Bindings.Ogre.EnumRenderOperationOperationType
import HGamer3D.Bindings.Ogre.StructHG3DClass
import HGamer3D.Bindings.Ogre.EnumSceneManagerPrefabType
import HGamer3D.Bindings.Ogre.ClassWindowUtilsHG3D as WindowUtils

import HGamer3D.APIs.Base.Engine.Types
import HGamer3D.APIs.Base.Graphics3D.Basic3D
import qualified HGamer3D.APIs.Base.InputSystem.InputSystem as IS

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent

import System.Win32.Process
import System.Environment (getArgs)

-- | initializes the 3d graphics module, used internally by the Base API engine module
initGraphics3D :: String -- ^ Name of the window, displayed
			-> String  -- ^ SceneManager type used
			-> String -- ^ path to hgamer3d installation directory
			-> Bool -- ^ flag, show configuration dialogue
			-> Bool -- ^ flag, use directx
			-> Bool -- ^ flag, is logging enabled
			-> IO (Graphics3DSystem)
			
initGraphics3D windowName sceneManagerType hg3dpath fConfig fDX fLog = do

	let plugins = if fDX then hg3dpath ++ "\\config\\pluginsDX.cfg" else hg3dpath ++ "\\config\\plugins.cfg"
	let config = if fDX then hg3dpath ++ "\\config\\engineDX.cfg" else hg3dpath ++ "\\config\\engine.cfg"
	
	root <- Root.new plugins config ""
	lmgr <- LogManager.getSingletonPtr
	if not fLog then do
		newlog <- LogManager.createLog lmgr "SilentLog" True False True
		return ()
		else do
			newlog <- LogManager.createLog lmgr "hgamer3d-engine.log" True False False
			return ()
			
	fOk <- if fConfig then
				Root.showConfigDialog root
				else do
					fLoaded <- Root.restoreConfig root
					if not fLoaded then
						Root.showConfigDialog root
						else
							return True
								
	
--	fUAddResourceLocations "resources.cfg"
	renderWindow <-Root.initialise root True windowName ""
	
	-- Suppress logging unless, fLog
			
	sceneManager <- Root.createSceneManager root sceneManagerType "SceneManager"
	
	camera <- SceneManager.createCamera sceneManager "SimpleCamera"
	Frustum.setNearClipDistance camera 5.0
	Frustum.setFarClipDistance camera 5000.0
	

	viewport <- RenderTarget.addViewport renderWindow camera 0 0.0 0.0 1.0 1.0
	let bgColor = Colour 0.0 0.0 0.0 1.0
	Viewport.setBackgroundColour viewport bgColor
	
	height <- Viewport.getActualHeight viewport
	width <- Viewport.getActualWidth viewport
	
	Frustum.setAspectRatio camera ((fromIntegral width) / (fromIntegral height))
	
	tm <- TextureManager.getSingletonPtr
	TextureManager.setDefaultNumMipmaps tm 20
	
	rgm <- ResourceGroupManager.getSingletonPtr
	ResourceGroupManager.addResourceLocation rgm (hg3dpath ++ "\\media\\materials") "FileSystem" "General" False
--	ResourceGroupManager.addResourceLocation rgm (hg3dpath ++ "\\media\\Sinbad.zip") "Zip" "General" False
--	ResourceGroupManager.addResourceLocation rgm (hg3dpath ++ "\\media\\ywing.zip") "Zip" "General" False

	ResourceGroupManager.createResourceGroup rgm "Schemes" False
	ResourceGroupManager.createResourceGroup rgm "Imagesets" False
	ResourceGroupManager.createResourceGroup rgm "Fonts" False
	ResourceGroupManager.createResourceGroup rgm "Layouts" False
	ResourceGroupManager.createResourceGroup rgm "LookNFeel" False
	ResourceGroupManager.createResourceGroup rgm "LuaScripts" False
	ResourceGroupManager.createResourceGroup rgm "XMLSchemas" False
	
	ResourceGroupManager.addResourceLocation rgm (hg3dpath ++ "\\media\\gui\\schemes") "FileSystem" "Schemes" False
	ResourceGroupManager.addResourceLocation rgm (hg3dpath ++ "\\media\\gui\\imagesets") "FileSystem" "Imagesets" False
	ResourceGroupManager.addResourceLocation rgm (hg3dpath ++ "\\media\\gui\\fonts") "FileSystem" "Fonts" False
	ResourceGroupManager.addResourceLocation rgm (hg3dpath ++ "\\media\\gui\\layouts") "FileSystem" "Layouts" False
	ResourceGroupManager.addResourceLocation rgm (hg3dpath ++ "\\media\\gui\\looknfeel") "FileSystem" "LookNFeel" False
	ResourceGroupManager.addResourceLocation rgm (hg3dpath ++ "\\media\\gui\\lua_scripts") "FileSystem" "LuaScripts" False
	ResourceGroupManager.addResourceLocation rgm (hg3dpath ++ "\\media\\gui\\xml_schemas") "FileSystem" "XMLSchemas" False

	
	cm <- ControllerManager.new
	WindowUtils.showCursor False
	messagePump <- MessagePump.new
	
	return (Graphics3DSystem root sceneManager rgm tm cm lmgr camera renderWindow viewport  messagePump )



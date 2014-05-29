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

-- Graphics3D/Internal/Base.hs

-- | Basic Types, Initialization and some global functions for the Graphics3D module. This is the internal implementation module. The external API is in HGamer3D.Graphics3D.
module HGamer3D.Graphics3D.Internal.Base (

  -- * Fundamental Types
  
  Object3D (..),
  Material (..),
  resourceMaterial,

  Mesh (..),
  Camera (..),
  
  -- * Internal Types, handling of Ogre Engine
  SceneManager (..),
  ResourceGroupManager (..),
  RootObject (..),
  Viewport (..),
  LogManager (..),
  TextureManager (..),
  RenderTarget (..),
  
  -- * The Graphics System
  Graphics3DSystem (..),


  -- * Initialization of Graphics3D and Game Loop Functions
  initGraphics3D,
  freeGraphics3D,
  stepGraphics3D,
  
  -- * global functions
  cameraLookAt,
  HGamer3D.Graphics3D.Internal.Base.setBackgroundColour,
  addResourceLocationMedia,
  addResourceZipfileMedia,
  addResourceLocationGUI,
  
  -- * Internal Functionality  
  graphics3DPumpWindowMessages,
  renderOneFrame,
  checkQuitReceived,
  _getNode
) 

where

import HGamer3D.Data
import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Window

import HGamer3D.Util

import qualified HGamer3D.Bindings.Ogre.ClassRoot as Root
import HGamer3D.Bindings.Ogre.ClassCamera as Camera
import HGamer3D.Bindings.Ogre.ClassNode as Node
import HGamer3D.Bindings.Ogre.ClassSceneManager as SceneManager
import HGamer3D.Bindings.Ogre.ClassResourceGroupManager as ResourceGroupManager
import HGamer3D.Bindings.Ogre.ClassTextureManager as TextureManager
import HGamer3D.Bindings.Ogre.ClassControllerManager as ControllerManager
import HGamer3D.Bindings.Ogre.ClassViewport as Viewport
import HGamer3D.Bindings.Ogre.ClassFrustum as Frustum
import HGamer3D.Bindings.Ogre.ClassAnimationState as AnimationState
import HGamer3D.Bindings.Ogre.ClassEntity as Entity
import HGamer3D.Bindings.Ogre.ClassControllerManager as ControllerManager
import HGamer3D.Bindings.Ogre.ClassLogManager as LogManager
import HGamer3D.Bindings.Ogre.ClassLog as Log
import HGamer3D.Bindings.Ogre.ClassHG3DUtilities as Util
import HGamer3D.Bindings.Ogre.ClassRenderTarget as RenderTarget
import HGamer3D.Bindings.Ogre.ClassManualObject as ManualObject
import HGamer3D.Bindings.Ogre.EnumRenderOperationOperationType
import HGamer3D.Bindings.Ogre.StructHG3DClass
import HGamer3D.Bindings.Ogre.EnumSceneManagerPrefabType
import HGamer3D.Bindings.Ogre.ClassWindowEventUtilities as WindowEventUtilities

import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.State.Class

import Control.Concurrent
import Data.Maybe

import HGamer3D.Data
import HGamer3D.Util

-- | The material. Define how an object is looking. Currently we have materials from resources
data Material = ResourceMaterial String -- ^ a named material, already installed as a resource

-- | give back a resource Material
resourceMaterial :: String           -- ^ the name of the Material as a resource
                    -> Material   -- ^ the created material
resourceMaterial name = ResourceMaterial name

-- | The mesh. A mesh in HGamer3D is just that, a mesh loaded from a template, a manual mesh, or loaded from file. It comes with default
-- material, but 3D objects based on this can get other materials individually.
data Mesh = CubeMesh | -- ^ Cube Mesh-Type
            PlaneMesh | -- ^ Plane Mesh-Type
            SphereMesh | -- ^ Sphere Mesh-Type
            ResourceMesh String | -- ^ Mesh resource loaded from file
            ManualMesh String  -- ^ Manual Mesh-Type, identified by name

-- | The basic 3D object. Each object is a single instance, which can be displayed, moved, scaled, rotated, ...
-- Objects are created cloned from meshes (templates for ojbects) or manually, by manual object creation methods.
data Object3D = SingleObject3D Mesh HG3DClass | -- ^ a single object, consisting of a Mesh and a Node (Ogre)
                CombinedObject3D [Object3D] HG3DClass -- ^ a combined object,consisting of many objects and a node for the combined one
				
instance Position Object3D where

	position obj = do
		pos <- Node.getPosition (_getNode obj)
		return (pos)
		
	positionTo obj pos = do
		Node.setPosition  (_getNode obj) pos
		return ()
	
instance Size Object3D where

	size obj = do
		pos <- Node.getScale  (_getNode obj)
		return (pos)
		
	sizeTo obj pos = do
		Node.setScale  (_getNode obj) pos
		return ()
	
instance Orientation Object3D where

	orientation obj = do
		q <- Node.getOrientation  (_getNode obj)
		let uq = mkNormal q
		return uq
	
	orientationTo obj uq = do
		Node.setOrientation  (_getNode obj) (fromNormal uq)
		return ()

instance Position Camera where

	position (Camera c) = do
		pos <- Camera.getPosition c
		return (pos)
		
	positionTo (Camera c) pos = do
		Camera.setPosition2 c  pos
		return ()
	
instance Orientation Camera where

	orientation (Camera c) = do
		q <- Camera.getOrientation c
		let uq = mkNormal q
		return uq
	
	orientationTo (Camera c) uq = do
		Camera.setOrientation c (fromNormal uq)
		return ()



data SceneManager = SceneManager HG3DClass
data ResourceGroupManager = ResourceGroupManager HG3DClass
data RootObject = RootObject HG3DClass
data Viewport = Viewport HG3DClass
data TextureManager = TextureManager HG3DClass
data LogManager = LogManager HG3DClass
data Camera = Camera HG3DClass
data RenderTarget = RenderTarget HG3DClass

data Graphics3DSystem = Graphics3DSystem {
  g3dsRoot :: RootObject,
  g3dsSceneManager :: SceneManager,
  g3dsResourceGroupManager :: ResourceGroupManager,
  g3dsLogManager :: LogManager,
  g3dsTextureManager :: TextureManager,
  g3dsRenderTarget :: RenderTarget,
  g3dsUniqueName :: UniqueName
}

-- | pump window messages for graphics
-- Not to be used, if WinEvent pollWinEvent or pumpWinEvents is used!
graphics3DPumpWindowMessages :: IO ()
graphics3DPumpWindowMessages = do
     WindowEventUtilities.messagePump
     return ()

-- | renders one frame on the screen
renderOneFrame :: Graphics3DSystem -> IO ()
renderOneFrame g3ds = do 
  let (RootObject root) = g3dsRoot g3ds
  Root.renderOneFrame root
  return ()

-- | steps the game loop by one tick, renders a frame and handles system messages
stepGraphics3D :: Graphics3DSystem -- ^ the graphics3d system
                  -> IO Bool -- ^ quit flag, true if window closed
stepGraphics3D g3ds = do
  renderOneFrame g3ds
  graphics3DPumpWindowMessages
  i <- checkQuitReceived
  return (i == 1)
  
freeGraphics3D :: Graphics3DSystem -> IO ()
freeGraphics3D g3ds = do
  let (RootObject root) = g3dsRoot g3ds
  let (RenderTarget rt) = g3dsRenderTarget g3ds
  Root.destroyRenderTarget root rt
  Root.delete root
  return ()

-- | initializes the 3d graphics module
initGraphics3D :: String -- ^ Name of the window, displayed
            -> String  -- ^ SceneManager type used
            -> Bool -- ^ flag, show configuration dialogue
            -> Bool -- ^ flag, is logging enabled
            -> IO (Graphics3DSystem, Camera, Viewport, Window)
            
initGraphics3D windowName sceneManagerType fConfig fLog  = do

        -- configuration path can be app user dir or local dir
        localDir <- getAppConfigDirectory
        appDir <- getExeConfigDirectory
        configFile <- findFileInDirs "engine.cfg" [localDir, appDir]
        pluginsFile <- findFileInDirs "plugins.cfg" [localDir, appDir]
        
        -- check both files exists
        let config = case configFile of
              Just cf -> cf
              Nothing -> error $ "HGamer3D - Graphics3D: could not find engine configuration file engine.cfg"
              
        let plugins = case pluginsFile of
              Just pf -> pf
              Nothing -> error $ "HGamer3D - Graphics3D: could not find plugins configuration file plugins.cfg"
              
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
        setupCloseEventHandler renderWindow
        windowHandle <- Util.getWindowHandle renderWindow
	
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
	
        -- resource locations, if path given, use this as base, if not use standard locations
        
        rgm <- ResourceGroupManager.getSingletonPtr
        
        ResourceGroupManager.createResourceGroup rgm "Schemes" False
        ResourceGroupManager.createResourceGroup rgm "Imagesets" False
        ResourceGroupManager.createResourceGroup rgm "Fonts" False
        ResourceGroupManager.createResourceGroup rgm "Layouts" False
        ResourceGroupManager.createResourceGroup rgm "LookNFeel" False
        ResourceGroupManager.createResourceGroup rgm "LuaScripts" False
        ResourceGroupManager.createResourceGroup rgm "XMLSchemas" False
        
        mediapath1 <- getAppMediaDirectory
        mediapath2 <- getExeMediaDirectory
          
        mapM (\mediapath -> do
                 ResourceGroupManager.addResourceLocation rgm (mediapath ++ osSep ++ "materials") "FileSystem" "General" False
                 ResourceGroupManager.addResourceLocation rgm (mediapath ++ osSep ++ "schemes") "FileSystem" "Schemes" False
                 ResourceGroupManager.addResourceLocation rgm (mediapath ++ osSep ++ "imagesets") "FileSystem" "Imagesets" False
                 ResourceGroupManager.addResourceLocation rgm (mediapath ++ osSep ++ "fonts") "FileSystem" "Fonts" False
                 ResourceGroupManager.addResourceLocation rgm (mediapath ++ osSep ++ "layouts") "FileSystem" "Layouts" False
                 ResourceGroupManager.addResourceLocation rgm (mediapath ++ osSep ++ "looknfeel") "FileSystem" "LookNFeel" False
                 ResourceGroupManager.addResourceLocation rgm (mediapath ++ osSep ++ "lua_scripts") "FileSystem" "LuaScripts" False
                 ResourceGroupManager.addResourceLocation rgm (mediapath ++ osSep ++ "xml_schemas") "FileSystem" "XMLSchemas" False
                 return ()) [mediapath1, mediapath2]
        

	ResourceGroupManager.initialiseAllResourceGroups rgm
        uniqueName <- createUniqueName "HG3DObj"
        
	return $ (Graphics3DSystem (RootObject root) (SceneManager sceneManager) (ResourceGroupManager rgm) (LogManager lmgr) (TextureManager tm) (RenderTarget renderWindow) uniqueName, (Camera camera), (Viewport viewport), (Window windowHandle)) 


-- | set the direction in a way, that the camera looks toward a specified point
        
cameraLookAt :: Camera -> Vec3 -> IO ()
cameraLookAt (Camera c) v = do
	Camera.lookAt c v
	return ()


-- specific single function
--

-- | sets the background colour of the 3d drawing window
setBackgroundColour :: Viewport -> Colour -> IO ()
setBackgroundColour (Viewport viewport) bgColour = do
	Viewport.setBackgroundColour viewport bgColour

-- locations of media in same folder as program resides
-- 

-- | adds a resource location for 3D media (Ogre)
addResourceLocationMedia :: Graphics3DSystem -- ^ the Graphics3D system object, returned by initGraphics3D
                            -> String -- ^ path to new resource location, the path should identify a directory
                            -> IO ()
addResourceLocationMedia g3ds path = do
        let (ResourceGroupManager rgm) = (g3dsResourceGroupManager g3ds)
	ResourceGroupManager.addResourceLocation rgm path "FileSystem" "General" False
	ResourceGroupManager.initialiseResourceGroup rgm "General"

-- | adds a resource location for 3D media (Ogre) which is a zip file
addResourceZipfileMedia :: Graphics3DSystem -- ^ the Graphics3D system object, returned by initGraphics3D 
                            -> String -- ^ path to new resource location, the path should identify a zip file
                           -> IO ()
addResourceZipfileMedia g3ds path = do
        let (ResourceGroupManager rgm) = (g3dsResourceGroupManager g3ds)
	ResourceGroupManager.addResourceLocation rgm path "Zip" "General" False
	ResourceGroupManager.initialiseResourceGroup rgm "General"

-- | adds a resource location for GUI media (CEGUI) which is a directory
addResourceLocationGUI :: Graphics3DSystem -- ^ the Graphics3D system object, returned by initGraphics3D
                          -> String -- ^ path to the directory with the GUI media in it
                          -> String -- ^ category of GUI media, for example: Layout, Images, ...
                          -> IO ()
addResourceLocationGUI g3ds path category = do
        let (ResourceGroupManager rgm) = (g3dsResourceGroupManager g3ds)
	ResourceGroupManager.addResourceLocation rgm path "FileSystem" category False
	ResourceGroupManager.initialiseResourceGroup rgm category

_getNode :: Object3D -> HG3DClass
_getNode (SingleObject3D entity node) = node
_getNode (CombinedObject3D objects node) = node


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

-- Graphics3D/Internal/Base.hs

module HGamer3D.Graphics3D.Internal.Base 
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
import HGamer3D.Bindings.Ogre.ClassSceneNode as SceneNode

import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.State.Class
import qualified System.Info as SI

import Control.Concurrent
import Data.Maybe

import HGamer3D.Data
import HGamer3D.Util

import HGamer3D.Graphics3D.Schema.Material
import HGamer3D.Graphics3D.Schema.Geometry
import HGamer3D.Graphics3D.Schema.Figure
import qualified HGamer3D.Graphics3D.Schema.Scene as Sc
import qualified HGamer3D.Graphics3D.Schema.Camera as Cam

data SceneManager = SceneManager HG3DClass
data ResourceGroupManager = ResourceGroupManager HG3DClass
data RootObject = RootObject HG3DClass
data TextureManager = TextureManager HG3DClass
data LogManager = LogManager HG3DClass
data RenderTarget = RenderTarget HG3DClass

-- | This data type holds the internal pointers to implementation objects and some additional state
-- like a unique name generator for implementation purposes.
data Graphics3DSystem = Graphics3DSystem {
  g3dsRoot :: RootObject,
  g3dsSceneManager :: SceneManager,
  g3dsResourceGroupManager :: ResourceGroupManager,
  g3dsLogManager :: LogManager,
  g3dsTextureManager :: TextureManager,
  g3dsRenderTarget :: RenderTarget,
  g3dsUniqueName :: UniqueName
}

-- | initializes the 3d graphics module
initGraphics3D :: String -- ^ Name of the window, displayed
            -> String  -- ^ SceneManager type used
            -> Bool -- ^ flag, show configuration dialogue
            -> Bool -- ^ flag, is logging enabled
            -> IO (Graphics3DSystem, Window)
           
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
{-	
	camera <- SceneManager.createCamera sceneManager "SimpleCamera"
	Frustum.setNearClipDistance camera 5.0
	Frustum.setFarClipDistance camera 5000.0
	

	viewport <- RenderTarget.addViewport renderWindow camera 0 0.0 0.0 1.0 1.0
	let bgColor = Colour 0.0 0.0 0.0 1.0
	Viewport.setBackgroundColour viewport bgColor
	
	height <- Viewport.getActualHeight viewport
	width <- Viewport.getActualWidth viewport
	
	Frustum.setAspectRatio camera ((fromIntegral width) / (fromIntegral height))
-}	
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
        
	return $ (Graphics3DSystem (RootObject root) (SceneManager sceneManager) (ResourceGroupManager rgm) (LogManager lmgr) (TextureManager tm) (RenderTarget renderWindow) uniqueName, (Window windowHandle)) 


-- | steps the game loop by one tick, renders a frame and handles system messages
stepGraphics3D :: Graphics3DSystem -- ^ the graphics3d system
                  -> IO Bool -- ^ quit flag, true if window closed
stepGraphics3D g3ds = do
  renderOneFrame g3ds
  -- this one is quite tricky, on Linux we need to call the message loop in addition to WinEvent!
  if SI.os /= "mingw32" then graphics3DPumpWindowMessages else return ()
  graphics3DPumpWindowMessages
  i <- checkQuitReceived
  return (i == 1)

-- | frees resources and shutdown the Graphics3D sub-system
freeGraphics3D :: Graphics3DSystem -> IO ()
freeGraphics3D g3ds = do
  let (RootObject root) = g3dsRoot g3ds
  let (RenderTarget rt) = g3dsRenderTarget g3ds
  Root.destroyRenderTarget root rt
  Root.delete root
  return ()

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


-- | Typed Ogre Classes: Node
data ONode = ON HG3DClass    -- this object is an Ogre Node

-- | Typed Ogre Classes: Entity
data OEntity = OE HG3DClass  -- this object is an Ogre Entity

-- | Typed Ogre Classes: Light
data OLight = OL HG3DClass -- this object is an Ogre Light

-- | Typed Ogre Classes: Camera
data OCamera = OC HG3DClass -- this object is an Ogre Camera

-- | Typed Ogre Classes: Material
data OMaterial = OM HG3DClass -- this object is an Ogre Material


-- | Typeclass for types, which have a main Node
class HasNode a where
  getNode :: a -> ONode
  getNode' :: a -> HG3DClass
  getNode' = (\(ON n) -> n) . getNode
  setNodePos :: a -> Position -> IO ()
  setNodePos hn pos = Node.setPosition (getNode' hn) pos
  setNodeOri :: a -> Orientation -> IO ()
  setNodeOri hn ori = Node.setOrientation (getNode' hn) (fromNormal ori)
  setNodeSiz :: a -> Size -> IO ()
  setNodeSiz hn siz = Node.setScale (getNode' hn) siz

instance HasNode ONode where
  getNode n = n

-- | Typeclass for objects which can be attached to Nodes
class NodeContent a where
  attachToNode :: a -> ONode -> IO ()
  detachFromNode :: a -> ONode -> IO ()

instance NodeContent OEntity where
  attachToNode (OE en) (ON n) = SceneNode.attachObject n en
  detachFromNode (OE en) (ON n) = SceneNode.detachObject2 n en

instance NodeContent OCamera where
  attachToNode (OC en) (ON n) = SceneNode.attachObject n en
  detachFromNode (OC en) (ON n) = SceneNode.detachObject2 n en

instance NodeContent OLight where
  attachToNode (OL en) (ON n) = SceneNode.attachObject n en
  detachFromNode (OL en) (ON n) = SceneNode.detachObject2 n en


-- |
-- For the data driven API, we need an internal representation, which
-- holds state inside the engine for outside defined data items (see Schema).
-- The following data definitions provide the framework for capturing
-- engine state for Figure and Geometry objects.
--
-- The EngineData Type holds a tree of items, corresponding to a Figure schema
-- data item, which also holds a corresponding pure data tree.
data EngineData = EDEntityNode OEntity ONode
              | EDNodeAndSub ONode [EngineData]

-- | A 3D Object represents the state inside the engine for a Schema 3D data item
data Object3D a = Object3D EngineData a

-- |
-- The Graphics3DItem TypeClass provides functions, which create, modify and delete objects in
-- the 3D world by simple data definitions. Instances of this TypeClass provide the implementations
-- behind for various 3D objects in the 3D world.
class Graphics3DItem a where
  object3D :: Graphics3DSystem -> a -> IO (Object3D a)
  update3D :: Graphics3DSystem -> Object3D a -> a -> IO (Object3D a)
  remove3D :: Graphics3DSystem -> (Object3D a) -> IO ()

instance HasNode EngineData where
  getNode (EDEntityNode _ node) = node
  getNode (EDNodeAndSub node _) = node

instance HasNode (Object3D a) where
  getNode (Object3D (EDEntityNode _ node) _) = node
  getNode (Object3D (EDNodeAndSub node _) _) = node

_getRootNode :: Graphics3DSystem -> IO ONode
_getRootNode g3ds = do
  let (SceneManager scm) = (g3dsSceneManager g3ds)
  rootNode <- SceneManager.getRootSceneNode scm
  return (ON rootNode)

_createSubNode :: ONode -> IO ONode
_createSubNode (ON parent) = SceneNode.createChildSceneNode parent zeroVec3 unitQ >>= (return . ON)

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

setSceneParameter :: Graphics3DSystem -> Sc.SceneParameter -> IO ()
setSceneParameter g3ds scene = do
  let (SceneManager scm) = g3dsSceneManager g3ds
  let (Sc.SceneParameter aLightColour shadow sky) = scene
  -- set ambient Light
  SceneManager.setAmbientLight scm aLightColour
  -- set Shadow Mode to be done

  -- set skybox
  case sky of
    Sc.NoSky -> do
      SceneManager.setSkyBoxEnabled scm False
      SceneManager.setSkyDomeEnabled scm False
    Sc.SkyBox (ResourceMaterial mname) distance -> do
      SceneManager.setSkyDomeEnabled scm False
      SceneManager.setSkyBox scm True mname distance True unitQ "General"
    Sc.SkyDome (ResourceMaterial mname) curvature tiling distance -> do
      SceneManager.setSkyBoxEnabled scm False
      SceneManager.setSkyDome scm True mname curvature tiling distance True unitQ 16 16 (-1) "General"

  return ()

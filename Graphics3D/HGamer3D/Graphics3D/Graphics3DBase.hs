{-# Language FlexibleInstances #-}
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

module HGamer3D.Graphics3D.Graphics3DBase 
where

import HGamer3D.Data
import HGamer3D.Common

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Window


import qualified HGamer3D.Bindings.Ogre.ClassRoot as Root
import HGamer3D.Bindings.Ogre.ClassCamera as Camera
import HGamer3D.Bindings.Ogre.ClassLight as Light
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
import HGamer3D.Bindings.Ogre.EnumLightType
import HGamer3D.Bindings.Ogre.ClassWindowEventUtilities as WindowEventUtilities
import HGamer3D.Bindings.Ogre.ClassSceneNode as SceneNode

import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.State.Class
import qualified System.Info as SI

import Control.Concurrent
import Data.Maybe

import qualified HGamer3D.Graphics3D.Graphics3DSchema as Sc

{- ----------------------------------------------------------------
   Basic Data Types
   ---------------------------------------------------------------- -}

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


{- ----------------------------------------------------------------
   Initialization and Runtime
   ---------------------------------------------------------------- -}


-- | initializes the 3d graphics module
initGraphics3D :: String -- ^ Name of the window, displayed
            -> String  -- ^ SceneManager type used
            -> Bool -- ^ flag, show configuration dialogue
            -> Bool -- ^ flag, is logging enabled
            -> IO (Graphics3DSystem, Window)
           
initGraphics3D windowName sceneManagerType fConfig fLog  = do

        -- configuration path can be app user dir or local dir
        appDir <- getAppConfigDirectory
        configFile <- findFileInDirs "engine.cfg" [appDir]
        pluginsFile <- findFileInDirs "plugins.cfg" [appDir]
        
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
        ResourceGroupManager.createResourceGroup rgm "Meshes" False
        ResourceGroupManager.createResourceGroup rgm "LookNFeel" False
        ResourceGroupManager.createResourceGroup rgm "LuaScripts" False
        ResourceGroupManager.createResourceGroup rgm "XMLSchemas" False
        
        mediapath1 <- getAppMediaDirectory
        -- mediapath2 <- getExeMediaDirectory
          
        mapM (\mediapath -> do
                 ResourceGroupManager.addResourceLocation rgm (mediapath ++ osSep ++ "materials") "FileSystem" "General" False
                 ResourceGroupManager.addResourceLocation rgm (mediapath ++ osSep ++ "schemes") "FileSystem" "Schemes" False
                 ResourceGroupManager.addResourceLocation rgm (mediapath ++ osSep ++ "imagesets") "FileSystem" "Imagesets" False
                 ResourceGroupManager.addResourceLocation rgm (mediapath ++ osSep ++ "fonts") "FileSystem" "Fonts" False
                 ResourceGroupManager.addResourceLocation rgm (mediapath ++ osSep ++ "layouts") "FileSystem" "Layouts" False
                 ResourceGroupManager.addResourceLocation rgm (mediapath ++ osSep ++ "meshes") "FileSystem" "Meshes" False
                 ResourceGroupManager.addResourceLocation rgm (mediapath ++ osSep ++ "looknfeel") "FileSystem" "LookNFeel" False
                 ResourceGroupManager.addResourceLocation rgm (mediapath ++ osSep ++ "lua_scripts") "FileSystem" "LuaScripts" False
                 ResourceGroupManager.addResourceLocation rgm (mediapath ++ osSep ++ "xml_schemas") "FileSystem" "XMLSchemas" False
                 return ()) [mediapath1]
        

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

{- ----------------------------------------------------------------
   Camera
   ---------------------------------------------------------------- -}

-- |  Camera, internal data object for engine
data Camera = Camera {
  cameraCamObject :: HG3DClass,
  cameraViewportObject :: HG3DClass,
  cameraSchema :: Sc.Camera
 }

-- | add a camera, you probably want to do this at least once
addCamera :: Graphics3DSystem -- ^ the graphics system
             -> Sc.Camera   -- ^ the Schema data for the camera
             -> IO Camera    -- ^ the resulting engine object
addCamera g3ds schema = do
  let (SceneManager sceneManager) = g3dsSceneManager g3ds
  let uname = g3dsUniqueName g3ds
  let (RenderTarget renderWindow) = g3dsRenderTarget g3ds
  let Sc.Camera (Sc.Frustum nd fd fov) (Sc.Viewport z pos bgr) = schema
  -- create camera
  cameraName <- (nextUniqueName uname) >>= (\n -> return ("Camera"++n))
  camera <- SceneManager.createCamera sceneManager cameraName
  -- add Viewport
  viewport <- RenderTarget.addViewport renderWindow camera z (rectX pos) (rectY pos) (rectWidth pos) (rectHeight pos)
  -- set Viewport parameters
  Viewport.setBackgroundColour viewport bgr
  -- set Frustum parameters
  Frustum.setNearClipDistance camera nd
  Frustum.setFarClipDistance camera fd
  Frustum.setFOVy camera (fromAngle fov)
  -- create camera return value
  let cam = Camera camera viewport schema
  -- adapt aspect ratio
  cameraAdaptAspectRatio cam

  return cam

-- | remove a camera 
removeCamera :: Graphics3DSystem -> Camera -> IO ()
removeCamera g3ds (Camera camera viewport schema) = do
  let (SceneManager sceneManager) = g3dsSceneManager g3ds
  let (RenderTarget renderWindow) = g3dsRenderTarget g3ds
  let Sc.Camera (Sc.Frustum nd fd fov) (Sc.Viewport z pos bgr) = schema
  RenderTarget.removeViewport renderWindow z
  SceneManager.destroyCamera sceneManager camera

-- | update an existing camera with new parameters
updateCamera :: Graphics3DSystem -> Camera -> Sc.Camera -> IO Camera
updateCamera g3ds cam@(Camera camera viewport schema) schema' = do
 
  let Sc.Camera (Sc.Frustum nd fd fov) (Sc.Viewport z pos bgr) = schema
  let Sc.Camera (Sc.Frustum nd' fd' fov') (Sc.Viewport z' pos' bgr') = schema'
  -- if zorder or position are not equal, we need to rebuild
  if (z /= z') || (pos /= pos') then do
    removeCamera g3ds cam
    addCamera g3ds schema'
    else do
      -- adapt single values, as needed
      if nd /= nd' then Frustum.setNearClipDistance camera nd' else return ()
      if fd /= fd' then Frustum.setFarClipDistance camera fd' else return ()
      if fov /= fov' then Frustum.setFOVy camera (fromAngle fov') else return ()
      if bgr /= bgr' then Viewport.setBackgroundColour viewport bgr' else return ()
      return (Camera camera viewport schema')
  
-- | adapt the aspect ration, in case the window size and aspect ratio changes, this
--   is called inside the engine automatically.
cameraAdaptAspectRatio :: Camera -> IO ()
cameraAdaptAspectRatio cam = do
  let (Camera camera viewport schema) = cam
  height <- Viewport.getActualHeight viewport
  width <- Viewport.getActualWidth viewport
  Frustum.setAspectRatio camera ((fromIntegral width) / (fromIntegral height))
  return ()
	
instance HasPosition Camera where
	position (Camera c _ _) = Camera.getPosition c
	positionTo (Camera c _ _) pos = Camera.setPosition2 c  pos
	
instance HasOrientation Camera where
	orientation (Camera c _ _) = do
		q <- Camera.getOrientation c
		let uq = mkNormal q
		return uq
	orientationTo (Camera c _ _) uq = do
		Camera.setOrientation c (fromNormal uq)
		return ()

-- | set the direction in a way, that the camera looks toward a specified point
cameraLookAt :: Camera -> Vec3 -> IO ()
cameraLookAt (Camera c _ _) v = do
	Camera.lookAt c v
	return ()

-- | Background colour of the 3d drawing window
setBackgroundColour :: Graphics3DSystem -> Camera -> Colour -> IO Camera
setBackgroundColour g3ds cam@(Camera camera viewport schema) bgColour = do
  let Sc.Camera (Sc.Frustum nd fd fov) (Sc.Viewport z pos bgr) = schema
  let schema' = Sc.Camera (Sc.Frustum nd fd fov) (Sc.Viewport z pos bgColour)
  updateCamera g3ds cam schema'

{- ----------------------------------------------------------------
   Light
   ---------------------------------------------------------------- -}

-- | The light.
data Light = Light ONode OLight Sc.Light

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

addLight :: Graphics3DSystem -> Sc.Light -> IO Light
addLight g3ds schema = do
  let (SceneManager scm) = (g3dsSceneManager g3ds)
  let (Sc.Light diffuse@(Colour r g b _) specular@(Colour r' g' b' _) ltype) = schema
  lightName <- nextUniqueName (g3dsUniqueName g3ds)
  light <- SceneManager.createLight scm lightName
  Light.setDiffuseColour light r g b 
  Light.setSpecularColour light r' g' b' 
  case ltype of      
    Sc.PointLight -> Light.setType light LT_POINT
    Sc.DirectionalLight dir -> do
      Light.setType light LT_DIRECTIONAL
      Light.setDirection2 light dir
    Sc.SpotLight dir inner outer -> do
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

updateLight :: Graphics3DSystem -> Light -> Sc.Light -> IO Light
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
  let schema = Sc.Light diffuse specular Sc.PointLight
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
  let schema = Sc.Light diffuse specular (Sc.SpotLight dir inner outer)
  light <- addLight g3ds schema
  positionTo light pos
  return $ light

-- | sets spotlight direction
spotLightSetDirection :: Graphics3DSystem -> Light -> Vec3 -> IO Light
spotLightSetDirection g3ds light dir' = do
  let (Light n l schema) = light
  let (Sc.Light diffuse@(Colour r g b _) specular@(Colour r' g' b' _) ltype) = schema
  case ltype of
    (Sc.SpotLight dir inner outer) -> updateLight g3ds light (Sc.Light diffuse specular (Sc.SpotLight dir' inner outer))
    _ -> return light

-- | creates a directional light at a specific location
directionalLight :: Graphics3DSystem -- ^ the Graphics3D system object, returned by initGraphics3D
                    -> Colour -- ^ diffuse Colour of the light
                    -> Colour -- ^ specular Colour of light
                    -> Vec3 -- ^ direction of light
                    -> IO (Light) -- ^ The light object
directionalLight g3ds diffuse specular dir = do
  let schema = Sc.Light diffuse specular (Sc.DirectionalLight dir)
  light <- addLight g3ds schema
  return $ light


{- -------------------------------------------------------------------------------
   Implemnentation for Platon Geometries
   ------------------------------------------------------------------------------- -}


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
ikosaederE :: Graphics3DSystem -- ^ the Graphics3D system object, returned by initGraphics3D
                       -> Colour      -- ^ colour of the ikosaeder
                       -> IO HG3DClass  -- ^ created mesh object as entity
ikosaederE g3ds colour = do
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
dodekaederE :: Graphics3DSystem -- ^ the Graphics3D system object, returned by initGraphics3D
                        -> Colour -- ^ colour of the dodekaeder
                        -> IO HG3DClass -- ^ created dodekaeder mesh as entity
dodekaederE g3ds colour = do
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

  
{- -------------------------------------------------------------------------------
   EngineItem for Geometry
   ------------------------------------------------------------------------------- -}

-- helper functions

_createGeometry :: Graphics3DSystem -> Sc.Geometry -> IO OEntity
_createGeometry g3ds geo = do
  let (SceneManager scm) = (g3dsSceneManager g3ds)
  entity <- case geo of
    Sc.ResourceGeometry s -> SceneManager.createEntity3 scm s
    Sc.Cube -> SceneManager.createEntity6 scm PT_CUBE
    Sc.Sphere -> SceneManager.createEntity6 scm PT_SPHERE
    Sc.Ikosaeder -> ikosaederE g3ds white
    Sc.Dodekaeder -> dodekaederE g3ds white
    Sc.Plane -> SceneManager.createEntity6 scm PT_PLANE
    _ -> error "HGamer3D.Graphics3D.Internal.Shapes._createMesh: Geo not implemented"
  let rE = OE entity
  if (geo /= Sc.Dodekaeder) && (geo /= Sc.Ikosaeder) then
      _buildTV rE
      else return ()
  return $ rE

_buildTV :: OEntity -> IO ()
_buildTV (OE entity) = Util.buildTangentVectors entity


{- -------------------------------------------------------------------------------
   EngineItem for Figure
   ------------------------------------------------------------------------------- -}

_createResourceFigure :: Graphics3DSystem -> String -> IO OEntity
_createResourceFigure g3ds name = do
  let (SceneManager scm) = (g3dsSceneManager g3ds)
  -- create the entity from the mesh
  entity <- SceneManager.createEntity3 scm name
  return $ OE entity

_setMaterial :: OEntity -> Sc.Material -> IO ()
_setMaterial (OE entity) material = do
  case material of
    (Sc.ResourceMaterial name) -> do
	Entity.setMaterialName entity name "General"
    _ -> error "HGamer3D.Graphics3D.Internal.Shapes._setMaterial: Material not implemented"

_setNodePOS n pos ori siz = do
  setNodePos n pos
  setNodeOri n ori
  setNodeSiz n siz
  
_createFigure :: Graphics3DSystem -> ONode -> Sc.Figure -> IO (EngineData)
_createFigure g3ds parent fig = do
  node <- _createSubNode parent
  case fig of
    Sc.SimpleFigure geo mat -> do
      meshEntity <- _createGeometry g3ds geo
      _setMaterial meshEntity mat
      _addEntityToNode g3ds node meshEntity
      return (EDEntityNode meshEntity node)
    Sc.ResourceFigure name -> do
      meshEntity <- _createResourceFigure g3ds name
      _buildTV meshEntity
      _addEntityToNode g3ds node meshEntity
      return (EDEntityNode meshEntity node)
    Sc.CombinedFigure arrSubs -> do
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


_updateFigure :: Graphics3DSystem -> ONode -> EngineData -> Sc.Figure -> Sc.Figure -> IO (EngineData)
_updateFigure g3ds parent edata oldFig newFig = do
  let node = getNode edata

  -- check, which action is needed
  
  let action = case oldFig of
        Sc.SimpleFigure oldGeo oldMat -> case newFig of
          Sc.SimpleFigure newGeo newMat -> if newGeo == oldGeo
                                           then
                                             if newMat == oldMat
                                                then DoNothing
                                                else ResetMaterial
                                           else ResetGeometry
          Sc.ResourceFigure name -> ResetResource
          Sc.CombinedFigure _ -> Rebuild
        Sc.ResourceFigure oldName -> case newFig of
          Sc.ResourceFigure newName -> if newName == oldName
                                       then DoNothing
                                       else ResetResource
          Sc.SimpleFigure newGeo newMat -> ResetGeometry
          Sc.CombinedFigure _ -> Rebuild
        Sc.CombinedFigure oldSubs -> case newFig of
          Sc.SimpleFigure _ _ -> Rebuild
          Sc.ResourceFigure _ -> Rebuild
          Sc.CombinedFigure newSubs -> if (length oldSubs) == (length newSubs)
                                       then CompareSub
                                       else Rebuild
          
  edataNew <-
        
        -- perform the single actions, we can keep existing node and single EngineData item form
        if action == ResetMaterial || action == ResetGeometry || action == ResetResource
           then do
             let (EDEntityNode eOld nOld) = edata
             case action of
               ResetMaterial -> do
                 let (Sc.SimpleFigure geo mat) = newFig
                 _setMaterial eOld mat
                 return edata
               ResetGeometry -> do
                 let (Sc.SimpleFigure geo mat) = newFig
                 eNew <- _createGeometry g3ds geo
                 _setMaterial eNew mat
                 _exchangeEntityInNode g3ds nOld eOld eNew
                 return (EDEntityNode eNew nOld)
               ResetResource -> do
                 let (Sc.ResourceFigure name) = newFig
                 eNew <- _createResourceFigure g3ds name
                 -- _buildTV eNew
                 _exchangeEntityInNode g3ds nOld eOld eNew
                 return (EDEntityNode eNew nOld)

           -- perform the actions, we compare two equal length subarrays
           else if action == CompareSub
             then do
               let (Sc.CombinedFigure oldSubs) = oldFig
               let (Sc.CombinedFigure newSubs) = newFig
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
      
instance Graphics3DItem Sc.Figure where
  
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

sphere :: Graphics3DSystem -> Float -> Sc.Material -> IO (Object3D Sc.Figure)
sphere g3ds radius material = object3D g3ds (Sc.CombinedFigure [
                 (zeroVec3, unitU, Vec3 radius radius radius,
                  Sc.SimpleFigure Sc.Sphere material) ])

cube :: Graphics3DSystem -> Float -> Sc.Material -> IO (Object3D Sc.Figure)
cube g3ds len material = object3D g3ds (Sc.CombinedFigure [
                 (zeroVec3, unitU, Vec3 len len len,
                  Sc.SimpleFigure Sc.Cube material) ])

cuboid :: Graphics3DSystem -> Vec3 -> Sc.Material -> IO (Object3D Sc.Figure)
cuboid g3ds vec material = object3D g3ds (Sc.CombinedFigure [
                 (zeroVec3, unitU, vec, Sc.SimpleFigure Sc.Cube material) ])
                           
dodekaeder :: Graphics3DSystem -> Float -> Sc.Material -> IO (Object3D Sc.Figure)
dodekaeder g3ds len material = object3D g3ds (Sc.CombinedFigure [
                 (zeroVec3, unitU, Vec3 len len len, Sc.SimpleFigure Sc.Dodekaeder material) ])
                           
ikosaeder :: Graphics3DSystem -> Float -> Sc.Material -> IO (Object3D Sc.Figure)
ikosaeder g3ds len material = object3D g3ds (Sc.CombinedFigure [
                 (zeroVec3, unitU, Vec3 len len len, Sc.SimpleFigure Sc.Ikosaeder material) ])
                           
instance HasPosition (Object3D Sc.Figure)  where
	position obj = Node.getPosition (getNode' obj)
	positionTo obj pos = Node.setPosition  (getNode' obj) pos
	
instance HasSize (Object3D Sc.Figure) where
	size obj = Node.getScale  (getNode' obj)
	sizeTo obj pos = Node.setScale  (getNode' obj) pos
	
instance HasOrientation (Object3D Sc.Figure) where
	orientation obj = do
		q <- Node.getOrientation  (getNode' obj)
		let uq = mkNormal q
		return uq
	orientationTo obj uq = do
		Node.setOrientation  (getNode' obj) (fromNormal uq)
		return ()
                           






{- ----------------------------------------------------------------
   ECS Stuff, is this needed any longer ???
   ---------------------------------------------------------------- -}



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
    Sc.SkyBox (Sc.ResourceMaterial mname) distance -> do
      SceneManager.setSkyDomeEnabled scm False
      SceneManager.setSkyBox scm True mname distance True unitQ "General"
    Sc.SkyDome (Sc.ResourceMaterial mname) curvature tiling distance -> do
      SceneManager.setSkyBoxEnabled scm False
      SceneManager.setSkyDome scm True mname curvature tiling distance True unitQ 16 16 (-1) "General"

  return ()

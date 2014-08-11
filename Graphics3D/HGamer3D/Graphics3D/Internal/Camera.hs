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

-- Graphics3D/Internal/Camera.hs

module HGamer3D.Graphics3D.Internal.Camera
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
import qualified System.Info as SI

import Control.Concurrent
import Data.Maybe

import HGamer3D.Data
import HGamer3D.Util

import HGamer3D.Graphics3D.Schema.Material
import HGamer3D.Graphics3D.Schema.Geometry
import HGamer3D.Graphics3D.Schema.Figure
import qualified HGamer3D.Graphics3D.Schema.Camera as Cam
import HGamer3D.Graphics3D.Internal.Base


{- ----------------------------------------------------------------
   Camera
   ---------------------------------------------------------------- -}

-- |  Camera, internal data object for engine
data Camera = Camera {
  cameraCamObject :: HG3DClass,
  cameraViewportObject :: HG3DClass,
  cameraSchema :: Cam.Camera
 }

-- | add a camera, you probably want to do this at least once
addCamera :: Graphics3DSystem -- ^ the graphics system
             -> Cam.Camera   -- ^ the Schema data for the camera
             -> IO Camera    -- ^ the resulting engine object
addCamera g3ds schema = do
  let (SceneManager sceneManager) = g3dsSceneManager g3ds
  let uname = g3dsUniqueName g3ds
  let (RenderTarget renderWindow) = g3dsRenderTarget g3ds
  let Cam.Camera (Cam.Frustum nd fd fov) (Cam.Viewport z pos bgr) = schema
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
  let Cam.Camera (Cam.Frustum nd fd fov) (Cam.Viewport z pos bgr) = schema
  RenderTarget.removeViewport renderWindow z
  SceneManager.destroyCamera sceneManager camera

-- | update an existing camera with new parameters
updateCamera :: Graphics3DSystem -> Camera -> Cam.Camera -> IO Camera
updateCamera g3ds cam@(Camera camera viewport schema) schema' = do
 
  let Cam.Camera (Cam.Frustum nd fd fov) (Cam.Viewport z pos bgr) = schema
  let Cam.Camera (Cam.Frustum nd' fd' fov') (Cam.Viewport z' pos' bgr') = schema'
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
  let Cam.Camera (Cam.Frustum nd fd fov) (Cam.Viewport z pos bgr) = schema
  let schema' = Cam.Camera (Cam.Frustum nd fd fov) (Cam.Viewport z pos bgColour)
  updateCamera g3ds cam schema'


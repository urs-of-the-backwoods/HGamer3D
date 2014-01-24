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

-- Basic3D.hs

-- |Types and basic objects for 3D module of Base API.

module HGamer3D.APIs.Base.Graphics3D.Basic3D (

        -- * Types
	Position3D (..),
	Scale3D (..),
	translate3D,
	Direction3D (..),
	Orientation3D (..),

        -- * Camera 
	Camera (..),
	HGamer3D.APIs.Base.Graphics3D.Basic3D.getCamera,
	cameraLookAt,
        
        -- * Background Colour
	HGamer3D.APIs.Base.Graphics3D.Basic3D.setBackgroundColour,
	
        -- * Resource locations
        -- $audioresource
        
	addResourceLocationMedia,
	addResourceZipfileMedia,
	addResourceLocationGUI,
	finalizeResourceLocations		
)

where 

import HGamer3D.Data.Colour
import HGamer3D.Data.Vector
import HGamer3D.Data.Angle
import HGamer3D.Data.HG3DClass

import HGamer3D.Bindings.Ogre.ClassCamera as Camera
import HGamer3D.Bindings.Ogre.ClassViewport as Viewport
import HGamer3D.Bindings.Ogre.ClassResourceGroupManager as ResourceGroupManager

import HGamer3D.APIs.Base.Engine.Types

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent

-- | a type with a Position3D instance can be positioned
class Position3D t where

        -- | get position function
	position3D :: t -> MHGamer3D Vec3
        -- | set position function
	positionTo3D :: t -> Vec3 -> MHGamer3D ()
	
-- | move position function
translate3D :: Position3D t => t -> Vec3 -> MHGamer3D ()
translate3D t v = do
	p <- position3D t
	positionTo3D t ( v &+ p )
	return ()

-- | a type with a Scale3D instance can be scaled
class Scale3D t where	
	
        -- | get scale function
	scale3D :: t -> MHGamer3D Vec3
        -- | set scale function
	scaleTo3D :: t -> Vec3 -> MHGamer3D ()

-- | a type with a Direction3D instance can be oriented towards a point (Camera for example)
class Direction3D t where
        -- | get direction function
	direction3D :: t -> MHGamer3D Vec3
        -- | set direction function
	directionTo3D :: t -> Vec3 -> MHGamer3D ()

-- | a type with an Orientation3D instance can be oriented in space
class Orientation3D t where
        -- | get orientation function
	orientation3D :: t -> MHGamer3D UnitQuaternion
        -- | set orientation function
	orientationTo3D :: t -> UnitQuaternion -> MHGamer3D ()

-- Camera functions
--

-- | The Camera
data Camera = Camera HG3DClass

-- | get the camera object
getCamera :: MHGamer3D Camera
getCamera = do
	rs <- ask
	let g3s = graphics3DSystem rs
	let cam = Camera (g3sCamera g3s)
	return cam
	
instance Position3D Camera where

	position3D (Camera c) = do
		pos <- liftIO $ Camera.getPosition c
		return (pos)
		
	positionTo3D (Camera c) pos = do
		liftIO $ Camera.setPosition2 c  pos
		return ()
	
instance Direction3D Camera where

	direction3D (Camera c) = do
		d <- liftIO $ Camera.getDirection c
		return d
		
	directionTo3D (Camera c) v = do
		liftIO $ Camera.setDirection2 c v
	
instance Orientation3D Camera where

	orientation3D (Camera c) = do
		q <- liftIO $ Camera.getOrientation c
		let uq = mkNormal q
		return uq
	
	orientationTo3D (Camera c) uq = do
		liftIO $ Camera.setOrientation c (fromNormal uq)
		return ()

-- | set the direction in a way, that the camera looks toward a specified point
cameraLookAt :: Camera -> Vec3 -> MHGamer3D ()
cameraLookAt (Camera c) v = do
	liftIO $ Camera.lookAt c v
	return ()


-- specific single function
--

-- | sets the background colour of the 3d drawing window
setBackgroundColour :: Colour -> MHGamer3D ()
setBackgroundColour bgColour = do
	rs <- ask
	let g3s = graphics3DSystem rs
	liftIO $ Viewport.setBackgroundColour (g3sViewport g3s) bgColour

-- locations of media in same folder as program resides
-- 

-- | adds a resource location for 3D media (Ogre)
addResourceLocationMedia :: String -- ^ path to new resource location, the path should identify a directory
                            -> MHGamer3D ()
addResourceLocationMedia path = do
	rs <- ask
	let g3s = graphics3DSystem rs
	let cs = commonSystem rs
	let progPath = csProgPath cs
	let rgm = g3sResourceGroupManager g3s
	liftIO $ ResourceGroupManager.addResourceLocation rgm (progPath ++ "\\" ++ path) "FileSystem" "General" False

-- | adds a resource location for 3D media (Ogre) which is a zip file
addResourceZipfileMedia :: String -- ^ path to new resource location, the path should identify a zip file
                           -> MHGamer3D ()
addResourceZipfileMedia path = do
	rs <- ask
	let g3s = graphics3DSystem rs
	let cs = commonSystem rs
	let progPath = csProgPath cs
	let rgm = g3sResourceGroupManager g3s
	liftIO $ ResourceGroupManager.addResourceLocation rgm (progPath ++ "\\" ++ path) "Zip" "General" False

-- | adds a resource location for GUI media (CEGUI) which is a directory
addResourceLocationGUI :: String -- ^ path to the directory with the GUI media in it
                          -> String -- ^ category of GUI media, for example: Layout, Images, ...
                          -> MHGamer3D ()
addResourceLocationGUI path category = do
	rs <- ask
	let g3s = graphics3DSystem rs
	let cs = commonSystem rs
	let progPath = csProgPath cs
	let rgm = g3sResourceGroupManager g3s
	liftIO $ ResourceGroupManager.addResourceLocation rgm (progPath ++ "\\" ++ path) "FileSystem" category False

-- | finalize adding resource locations. This function needs to be called at the end of all addResourceLocation... functions.
finalizeResourceLocations :: MHGamer3D ()	
finalizeResourceLocations = do
	rs <- ask
	let g3s = graphics3DSystem rs
	let cs = commonSystem rs
	let rgm = g3sResourceGroupManager g3s
	liftIO $ ResourceGroupManager.initialiseAllResourceGroups rgm
	


{-$audioresource
The resource location for audio files is currently fixed and it is not needed, to set it. The path 
for audio files is \"..\/media\/audio\".
-}
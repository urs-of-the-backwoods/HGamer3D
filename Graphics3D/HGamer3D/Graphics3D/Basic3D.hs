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

-- Basic3D.hs

-- | Basic functions for the Graphics3D module
module HGamer3D.Graphics3D.Basic3D (


	cameraLookAt,
        
    -- * Background Colour
	HGamer3D.Graphics3D.Basic3D.setBackgroundColour,
	
    -- * Resource locations
       -- $audioresource
        
	addResourceLocationMedia,
	addResourceZipfileMedia,
	addResourceLocationGUI
)

where 

import HGamer3D.Data

import HGamer3D.Bindings.Ogre.ClassCamera as Camera
import HGamer3D.Bindings.Ogre.ClassViewport as Viewport
import HGamer3D.Bindings.Ogre.ClassResourceGroupManager as ResourceGroupManager

import HGamer3D.Graphics3D.Types

-- Camera functions
--


instance Position3D Camera where

	position3D (Camera c) = do
		pos <- Camera.getPosition c
		return (pos)
		
	positionTo3D (Camera c) pos = do
		Camera.setPosition2 c  pos
		return ()
	
instance Direction3D Camera where

	direction3D (Camera c) = do
		d <- Camera.getDirection c
		return d
		
	directionTo3D (Camera c) v = do
		Camera.setDirection2 c v
	
instance Orientation3D Camera where

	orientation3D (Camera c) = do
		q <- Camera.getOrientation c
		let uq = mkNormal q
		return uq
	
	orientationTo3D (Camera c) uq = do
		Camera.setOrientation c (fromNormal uq)
		return ()

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


{-
    Camera Datatype
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2011-2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Graphics3D/Camera.hs
-}

-- | Module providing the Camera type
module HGamer3D.Graphics3D.Camera

(
        Camera (..),
        ctCamera,

        Frustum (..),
        ctFrustum
)

where

import Data.MessagePack
import HGamer3D.Data
import HGamer3D.ECS

-- | The Camera Data Type
data Camera = FullViewCamera                            -- ^ a camera using the full view
              | OverlayCamera (Rectangle Float)         -- ^ a camera using only parts of the view (overlay)
              deriving (Eq, Show)

ctCamera :: ComponentType Camera
ctCamera = ComponentType 0xd3b0d455ab1f4716 

instance ComponentClass Camera where
        toObj FullViewCamera = ObjectArray [ObjectInt 0]
        toObj (OverlayCamera rec) = ObjectArray [ObjectInt 1, toObj rec]
        fromObj (ObjectArray [ObjectInt 0]) = FullViewCamera
        fromObj (ObjectArray [ObjectInt 1, rec_ob]) = OverlayCamera (fromObj rec_ob)

data Frustum = Frustum {
        frNearDistance :: Float,
        frFarDistance :: Float,
        frFieldOfViewHorizontal :: Angle
} deriving (Eq, Show)

instance ComponentClass Frustum where
        toObj (Frustum nd fd fovh) = ObjectArray [ObjectFloat nd, ObjectFloat fd, toObj fovh]
        fromObj (ObjectArray [ObjectFloat nd, ObjectFloat fd, fovh_o]) = Frustum nd fd (fromObj fovh_o)

ctFrustum :: ComponentType Frustum
ctFrustum = ComponentType 0xf3ce3235d4f8e73d





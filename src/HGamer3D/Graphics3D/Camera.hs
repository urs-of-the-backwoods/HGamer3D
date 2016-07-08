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
import Fresco

-- generated

data Camera = FullViewCamera
    | OverlayCamera (Rectangle Int) Float
    deriving (Eq, Show)

instance ComponentClass Camera where
    toObj (FullViewCamera) = ObjectArray [ObjectInt 0, ObjectArray []]
    toObj (OverlayCamera v1 v2) = ObjectArray [ObjectInt 1, ObjectArray [(toObj v1), ObjectFloat v2]]
    fromObj (ObjectArray [ObjectInt 0, ObjectArray []]) = FullViewCamera
    fromObj (ObjectArray [ObjectInt 1, ObjectArray [v1, ObjectFloat v2]]) = OverlayCamera (fromObj v1) v2

ctCamera :: ComponentType Camera
ctCamera = ComponentType 0xd3b0d455ab1f4716

-- generated

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





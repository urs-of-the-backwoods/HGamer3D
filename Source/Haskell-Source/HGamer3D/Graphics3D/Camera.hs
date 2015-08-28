-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011 - 2015 Peter Althainz
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

-- HGamer3D/Graphics3D/Camera.hs

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

data Camera = MainCamera
              | OverlayCamera (Rectangle Float)
              deriving (Eq, Show)

ctCamera :: ComponentType Camera
ctCamera = ComponentType 0xd3b0d455ab1f4716 

instance ComponentClass Camera where
        toObj MainCamera = ObjectArray [ObjectInt 0]
        toObj (OverlayCamera rec) = ObjectArray [ObjectInt 1, toObj rec]
        fromObj (ObjectArray [ObjectInt 0]) = MainCamera
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





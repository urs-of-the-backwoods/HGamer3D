{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
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

-- HGamer3D/Graphics3D/Schema/Camera.hs

-- | Types which describe the Scene, the high level of a Scene Tree
module HGamer3D.Graphics3D.Schema.Camera

where

import Data.Typeable
import HGamer3D.Data as Dat

-- | The data to specify a camera
data Camera = Camera { 
  frustum :: Frustum,     -- ^ Frustum of the camera ,
  viewport :: Viewport    -- ^ Where to draw on the window
                     } deriving (Eq, Show, Typeable)
                                
-- | The frustum defines the area from which objects are rendered into the camera
data Frustum = Frustum {
  nearDistance :: Float,    -- ^ clipping distance near, objects which are nearer are not shown
  farDistance :: Float,     -- ^ clipping distance far, objects, which are more away are not shown
  fieldOfViewHorizontal :: Dat.Angle  -- ^ The field of view angle horizontally, the vertical direction is automatically determined by the aspect ratio of the final viewport window.
  } deriving (Eq, Show, Typeable)

deriving instance Typeable1 Dat.Rectangle

data Viewport = Viewport {
  zOrder :: Int,              -- ^ Z order in case of overlapping Viewports, higher ZOrders are on top
  position :: Dat.Rectangle Float,   -- ^ position of viewport in Window, coordinates reaching from 0.0 to 1.0
  backgroundColor :: Colour
} deriving (Eq, Show, Typeable)
  


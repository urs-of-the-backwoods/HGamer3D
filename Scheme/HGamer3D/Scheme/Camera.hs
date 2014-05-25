{-# LANGUAGE FlexibleContexts, StandaloneDeriving, TemplateHaskell #-}

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

-- Camera.hs

-- | Types which describe the Scene, the high level of a Scene Tree
module HGamer3D.Scheme.Camera

where

import HGamer3D.Data as Dat
import HGamer3D.Util
import Control.Lens

-- | The camera type
data Camera = Camera { 
  _frustum :: Frustum  -- ^ Frustum of the camera 
                     } deriving (Eq, Show)

data Frustum = Frustum {
  _nearDistance :: Float,
  _farDistance :: Float,
  _fieldOfViewHorizontal :: Dat.Angle } deriving (Eq, Show)
               
$(makeLenses ''Camera)
$(makeLenses ''Frustum)
  


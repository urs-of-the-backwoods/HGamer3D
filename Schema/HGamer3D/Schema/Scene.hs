{-# LANGUAGE FlexibleContexts, StandaloneDeriving, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TemplateHaskell #-}

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

-- Scene.hs

-- | Types which describe the Scene, the high level of a Scene Tree
module HGamer3D.Schema.Scene

where

import HGamer3D.Data
import HGamer3D.Util

import Control.Lens

-- | The type of coordinate system
data CoordinateSystem = YUpRight -- ^ Y achsis up, right handed, like many 3D modelling programs and game engines (Ogre)
                      | ZUpRight -- ^ Z achsis up, right handed, like Blender and typical CAD programs
                      | YUpLeft -- ^ Y achsis up, z into the screen, this is a left handed coordinate system (exotic)
                      deriving (Eq, Show)

-- | The Scene, high level construct of scene tree
data Scene = Scene {
  _sceneCoordinate :: CoordinateSystem -- ^ the coordinate system, used in this scene
}  deriving (Eq, Show)

$(makePrisms ''CoordinateSystem)
$(makeFields ''Scene)
  


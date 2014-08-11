{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}
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

-- HGamer3D/Graphics3D/Schema/Light.hs

-- | Types which describe Light
module HGamer3D.Graphics3D.Schema.Light

where

import Data.Typeable

import HGamer3D.Data as Dat


-- | The Light data type


data Light = Light {
  diffuseColour :: Colour,
  specularColour :: Colour,
  lightType :: LightType
  } deriving (Eq, Show, Typeable)

data LightType = PointLight                  -- ^ A source shining from one location
               | DirectionalLight Vec3       -- ^ A source shining from one direction (like the sun)
               | SpotLight Vec3 Angle Angle  -- ^ A source shining from one location into one direction with a cone,
                                             --   angles are inner angle, outer angle (a flashlight).
                                             --   Angles should be between 5 and 355 degrees.
           deriving (Eq, Show, Typeable)
  




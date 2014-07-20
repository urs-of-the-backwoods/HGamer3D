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

-- HGamer3D/Graphics3D/Schema/Scene.hs

-- | Types which describe global Scene Parameters
module HGamer3D.Graphics3D.Schema.Scene

where

import Data.Typeable

import HGamer3D.Data as Dat
import HGamer3D.Graphics3D.Schema.Material

data SceneParameter = SceneParameter {
  ambientLight :: Colour,
  shadowType :: ShadowType,
  skyType :: SkyType
  } deriving (Eq, Show, Typeable)

data ShadowType = NoShadows
                | TextureAdditive
                | TextureModulative
                | StencilAdditive
                | StencilModulative
                  deriving (Eq, Show, Typeable)

data SkyType = NoSky
             | SkyBox Material Float                 -- Distance
             | SkyDome Material Float Float Float    -- Curvature, Tiling, Distance
               deriving (Eq, Show, Typeable)

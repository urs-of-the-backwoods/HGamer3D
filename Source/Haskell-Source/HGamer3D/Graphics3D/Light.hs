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

-- HGamer3D/Graphics3D/Light.hs

-- | Module providing the Light type
module HGamer3D.Graphics3D.Light

(
        Light (..),
        LightParameter (..),
        ctLight
)

where

import Data.MessagePack
import HGamer3D.Data

data LightParameter = LightParameter Float Float Float Bool -- brightness, range, spec intesity, per vertex
                      deriving (Eq, Show)

instance ComponentClass LightParameter where
    toObj (LightParameter br ra si pv) = ObjectArray [ObjectFloat br, ObjectFloat ra, ObjectFloat si, ObjectBool pv]
    fromObj (ObjectArray [ObjectFloat br, ObjectFloat ra, ObjectFloat si, ObjectBool pv]) = LightParameter  br ra si pv

data Light = PointLight LightParameter
             | DirectionalLight LightParameter
             | SpotLight LightParameter Angle Float -- fiel of view, aspect ratio
               deriving (Eq, Show)

instance ComponentClass Light where
    toObj (PointLight lp) = ObjectArray [ObjectInt 0, toObj lp]
    toObj (DirectionalLight lp) =  ObjectArray [ObjectInt 1, toObj lp]
    toObj (SpotLight lp fov ar) = ObjectArray [ObjectInt 2, toObj lp, toObj fov, ObjectFloat ar]

    fromObj (ObjectArray [ObjectInt 0, lp_o]) = PointLight (fromObj lp_o)
    fromObj (ObjectArray [ObjectInt 1, lp_o]) = DirectionalLight (fromObj lp_o)
    fromObj (ObjectArray [ObjectInt 2, lp_o, fov_o, ObjectFloat ar]) = SpotLight (fromObj lp_o) (fromObj fov_o) ar

ctLight :: ComponentType Light
ctLight = ComponentType 0x981e80e50d994ea9


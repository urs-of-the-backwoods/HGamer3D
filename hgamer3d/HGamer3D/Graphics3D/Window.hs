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

-- | Module providing the Window type
module HGamer3D.Graphics3D.Window

(
        WindowG3D (..),
        ctWGr,
        xyWindow,
        fullScreen
)

where

import Data.MessagePack
import Fresco
import HGamer3D.Data
-- import HGamer3D.ECS

data WindowG3D = WindowG3D {
    windowWidth::Int,
    windowHeight::Int,
    windowBorderless::Bool,
    windowFullScreen::Bool,
    windowResizable::Bool
}

instance ComponentClass WindowG3D where
    toObj (WindowG3D v1 v2 v3 v4 v5) = ObjectArray [(toObj v1), (toObj v2), ObjectBool v3, ObjectBool v4, ObjectBool v5]
    fromObj (ObjectArray [v1, v2, ObjectBool v3, ObjectBool v4, ObjectBool v5]) = WindowG3D (fromObj v1) (fromObj v2) v3 v4 v5


ctWGr :: ComponentType WindowG3D
ctWGr = ComponentType 0xed96843bfb53d99e

xyWindow x y = WindowG3D x y False False True
fullScreen = WindowG3D 0 0 True True False


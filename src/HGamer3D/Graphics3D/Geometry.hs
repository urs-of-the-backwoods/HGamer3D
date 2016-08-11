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

-- HGamer3D/Graphics3D/Geometry.hs

-- | Module providing the Geometry type
module HGamer3D.Graphics3D.Geometry

(
        Shape (..),
        Geometry (..),
        ctGeometry,
        ctGraphicsElement
)

where

import Data.MessagePack

import Fresco
import Data.Text

import HGamer3D.Data
import HGamer3D.Graphics3D.Material

-- generated

-- HGamer3D website, entities and events, Geometry data type and component
-- | A shape is a basic geometric formd
data Shape = Sphere
    | Cube
    | Plane
    | Cylinder
    | Pyramid
    | Torus
    deriving (Eq, Read, Show)

instance ComponentClass Shape where
    toObj (Sphere) = ObjectArray [ObjectInt 0, ObjectArray []]
    toObj (Cube) = ObjectArray [ObjectInt 1, ObjectArray []]
    toObj (Plane) = ObjectArray [ObjectInt 2, ObjectArray []]
    toObj (Cylinder) = ObjectArray [ObjectInt 3, ObjectArray []]
    toObj (Pyramid) = ObjectArray [ObjectInt 4, ObjectArray []]
    toObj (Torus) = ObjectArray [ObjectInt 5, ObjectArray []]
    fromObj (ObjectArray [ObjectInt 0, ObjectArray []]) = Sphere
    fromObj (ObjectArray [ObjectInt 1, ObjectArray []]) = Cube
    fromObj (ObjectArray [ObjectInt 2, ObjectArray []]) = Plane
    fromObj (ObjectArray [ObjectInt 3, ObjectArray []]) = Cylinder
    fromObj (ObjectArray [ObjectInt 4, ObjectArray []]) = Pyramid
    fromObj (ObjectArray [ObjectInt 5, ObjectArray []]) = Torus

data Geometry = ShapeGeometry Shape
    | ResourceGeometry Text
    deriving (Eq, Read, Show)

instance ComponentClass Geometry where
    toObj (ShapeGeometry v1) = ObjectArray [ObjectInt 0, ObjectArray [(toObj v1)]]
    toObj (ResourceGeometry v1) = ObjectArray [ObjectInt 1, ObjectArray [(toObj v1)]]
    fromObj (ObjectArray [ObjectInt 0, ObjectArray [v1]]) = ShapeGeometry (fromObj v1)
    fromObj (ObjectArray [ObjectInt 1, ObjectArray [v1]]) = ResourceGeometry (fromObj v1)

ctGeometry :: ComponentType Geometry
ctGeometry = ComponentType 0xee433d1a4b964591

ctGraphicsElement :: ComponentType ()
ctGraphicsElement = ComponentType 0x65114ba821671643
-- end of website text


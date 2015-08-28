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
        ctGeometry
)

where

import Data.MessagePack
import Data.Text

import HGamer3D.Data
import HGamer3D.Graphics3D.Material

-- | A shape is a basic geometric form
data Shape =   Sphere 
                | Cube
                | Plane
                deriving (Eq, Show)

instance ComponentClass Shape where
        toObj Sphere = ObjectInt 1
        toObj Cube = ObjectInt 2
        toObj Plane = ObjectInt 3
        fromObj (ObjectInt i) = case i of
                1 -> Sphere
                2 -> Cube
                3 -> Plane

-- | A geometry is the basic drawable item, it can be a shape or a mesh loaded from a resource
data Geometry = ShapeGeometry Shape
                | ResourceGeometry Text
                deriving (Eq, Show)

-- | Component Type for the Geometry
ctGeometry :: ComponentType Geometry
ctGeometry = ComponentType 0xee433d1a4b964591 

instance ComponentClass Geometry where
        toObj (ShapeGeometry sh) = ObjectArray [ObjectInt 1, toObj sh]
        toObj (ResourceGeometry mesh) = ObjectArray [ObjectInt 2, toObj mesh]
        fromObj (ObjectArray [ObjectInt 1, sh_ob])  = ShapeGeometry (fromObj sh_ob) 
        fromObj (ObjectArray [ObjectInt 2, mesh_o])  = ResourceGeometry (fromObj mesh_o)



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

-- Scene.hs

-- | Types which describe the Geometry 
module HGamer3D.Schema.Geometry

where

import qualified HGamer3D.Data as D

import Control.Lens

import HGamer3D.Data.Vector.Instances

-- | Geometry Type
data Geometry = MeshGeometry Mesh  -- ^ a Mesh as Geometry
              | LineGeometry Line  -- ^ a Line as Geometry
              | PrimitiveGeometry Primitive  -- ^ a Geometry Primitive, like Cube or Sphere
              deriving (Eq, Show)

-- | Primitive Geometries, Solids
data Primitive = Cuboid Float
                 | Cube D.Vec3
                 | Sphere Float  
                 | Cylinder D.Vec2
                 | Cone D.Vec2
                 | SquarePyramid D.Vec2
                 | TriangularPyramid D.Vec2
                 deriving (Eq, Show)

-- | A Mesh
data Mesh = ResourceMesh String 
            deriving (Eq, Show)
                     
-- | A Line
data Line = Line D.Vec3 D.Vec3 deriving (Eq, Show)
                     
$(makePrisms ''Geometry)
$(makePrisms ''Primitive)
$(makePrisms ''Mesh)
$(makePrisms ''Line)
  


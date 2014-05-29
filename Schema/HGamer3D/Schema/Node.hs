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

-- Node.hs

-- | Types which describe Materials
module HGamer3D.Schema.Node

where

import qualified HGamer3D.Data as Dat
import HGamer3D.Data.Vector.Instances
import qualified Data.Map as Map
import Control.Lens

import qualified HGamer3D.Schema.Material as Mat
import qualified HGamer3D.Schema.Texture as Tex
import qualified HGamer3D.Schema.Colour as Col
import qualified HGamer3D.Schema.Light as Li
import qualified HGamer3D.Schema.Camera as Cam
import qualified HGamer3D.Schema.Scene as Sc
import qualified HGamer3D.Schema.Geometry as Geo

deriving instance Eq Dat.UnitQuaternion


-- | a SceneNode, making up the scene hierarchy and their spacial layout
data Node = Node {
  _position :: Dat.Vec3,
  _size :: Dat.Vec3,
  _orientation :: Dat.UnitQuaternion,
  
  _materials :: Map.Map Mat.MaterialSlot Mat.Material,  -- ^ slots for Materials, referenced by Geometries
  _textures :: Map.Map Tex.TextureSlot Tex.Texture,     -- ^ slots for Textures, referenced by Materials and Geometries directly
  _colours :: Map.Map Col.ColourSlot Dat.Colour,        -- ^ slots for Colours, referenced by Materials and Geometries directly
  
  _attributes :: [NodeAttribute],                       -- ^ attributes are the "content" of the node
  _children :: [Node]                                   -- ^ child nodes, make up the hierarchy
  } deriving (Eq, Show)
            
data NodeAttribute = SceneAttribute Sc.Scene             -- ^ A node, which contains all scene info, like coordinate system
                   | CameraAttribute Cam.Camera          -- ^ Camera
                   | LightAttribute Li.Light             -- ^ Light
                   | GeometryAttribute Geo.Geometry      -- ^ All types of Geometry: Meshes, Lines, ...
                   deriving (Eq, Show)

$(makeLenses ''Node)
$(makePrisms ''NodeAttribute)
  


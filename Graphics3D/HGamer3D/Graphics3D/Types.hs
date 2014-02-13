{-# LANGUAGE FlexibleContexts #-}

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

-- Types.hs

-- | Basic types for the Graphics3D module
module HGamer3D.Graphics3D.Types (

  -- * Basic Engine Entities
  SceneManager (..),
  ResourceGroupManager (..),
  RootObject (..),
  Viewport (..),
  LogManager (..),
  TextureManager (..),
  Camera (..),
  RenderTarget (..),
  
  Graphics3DSystem (..),

  -- * Type Definitions
  Object3D (..),
  Material (..),
  Mesh (..),

) 

where

import HGamer3D.Data
import HGamer3D.Util

-- | The material. Define how an object is looking. Currently we have materials from resources
data Material = ResourceMaterial String -- ^ a named material, already installed as a resource

-- | The mesh. A mesh in HGamer3D is just that, a mesh loaded from a template, a manual mesh, or loaded from file. It comes with default
-- material, but 3D objects based on this can get other materials individually.
data Mesh = CubeMesh | -- ^ Cube Mesh-Type
            PlaneMesh | -- ^ Plane Mesh-Type
            SphereMesh | -- ^ Sphere Mesh-Type
            ResourceMesh String | -- ^ Mesh resource loaded from file
            ManualMesh String  -- ^ Manual Mesh-Type, identified by name

-- | The basic 3D object. Each object is a single instance, which can be displayed, moved, scaled, rotated, ...
-- Objects are created cloned from meshes (templates for ojbects) or manually, by manual object creation methods.
data Object3D = SingleObject3D Mesh HG3DClass | -- ^ a single object, consisting of a Mesh and a Node (Ogre)
                CombinedObject3D [Object3D] HG3DClass -- ^ a combined object,consisting of many objects and a node for the combined one
				
data SceneManager = SceneManager HG3DClass
data ResourceGroupManager = ResourceGroupManager HG3DClass
data RootObject = RootObject HG3DClass
data Viewport = Viewport HG3DClass
data TextureManager = TextureManager HG3DClass
data LogManager = LogManager HG3DClass
data Camera = Camera HG3DClass
data RenderTarget = RenderTarget HG3DClass

data Graphics3DSystem = Graphics3DSystem {
  g3dsRoot :: RootObject,
  g3dsSceneManager :: SceneManager,
  g3dsResourceGroupManager :: ResourceGroupManager,
  g3dsLogManager :: LogManager,
  g3dsTextureManager :: TextureManager,
  g3dsRenderTarget :: RenderTarget,
  g3dsUniqueName :: UniqueName
}


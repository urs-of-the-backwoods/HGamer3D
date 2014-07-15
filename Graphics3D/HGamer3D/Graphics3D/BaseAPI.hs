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

-- Graphics.hs

-- | 3D Graphics for HGamer3D, public API.
module HGamer3D.Graphics3D.BaseAPI

(
  -- * Basic Types for the Graphics3D System
  Graphics3DSystem,
  Object3D,
  Camera,

  -- * Initializing the Graphics3D Subsystem
  
  -- |
  -- The initialization, step and free functions are called from the HGamer3D corresponding functions, therefore
  -- they do not need to be called in code, which includes HGamer3D - BaseAPI.
  
  initGraphics3D,
  stepGraphics3D,
  freeGraphics3D,

  -- * Creating, Updating and Removing of 3D Objects
  Graphics3DItem (..),
  
  -- * Creating simple 3D objects
  sphere,
  cube,
  cuboid,
  ikosaeder,
  dodekaeder,

  -- * Light
  Light,
  addLight,
  updateLight,
  removeLight,
  
  HGamer3D.Graphics3D.Internal.Light.setAmbientLight,
  pointLight,
  spotLight,
  spotLightSetDirection,
  directionalLight,

  -- * Material
  Material (..),
  
  -- * Misc Functions

  addCamera,
  removeCamera,
  updateCamera,
  cameraLookAt,

  addResourceLocationMedia,
  addResourceZipfileMedia,
  addResourceLocationGUI,

)

where

  import HGamer3D.Data
  import HGamer3D.Util
  
  import HGamer3D.Graphics3D.Internal.Base
  import HGamer3D.Graphics3D.Internal.Light
  import HGamer3D.Graphics3D.Internal.Shapes
  
  import HGamer3D.Graphics3D.Schema.Material
  
--  import HGamer3D.Graphics3D.Internal.PlatonShapes
  




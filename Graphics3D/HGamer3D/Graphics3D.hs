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

-- Graphics.hs

-- | 3D Graphics for HGamer3D, public API.
module HGamer3D.Graphics3D

(
  -- * The Graphics System
  Graphics3DSystem (..),
  Camera,
  Viewport,

  -- * Initialization of Graphics3D and Game Loop Functions
  initGraphics3D,
  freeGraphics3D,
  stepGraphics3D,
  
  -- * The typeclass of (data) items, which can be put into the engine
  Engine3DItem (..),

  -- * The 3D object as outer visible part of it
  Object3D (..),

  sphere,
  cube,
  cuboid,

    -- * Light
  Light,
  HGamer3D.Graphics3D.Internal.Light.setAmbientLight,
  pointLight,
  spotLight,
  spotLightSetDirection,
  setSpotLightAngle,
  directionalLight,

  {-
  -- * Materials
  resourceMaterial,

  -- * Meshes
  sphereMesh,
  cubeMesh,
  planeMesh,
  resourceMesh,
        
  colouredCubeMesh,
  colouredLineMesh,
  rainbowCubeMesh,
	
  ikosaederMesh,
  dodekaederMesh,

  -- * Graphics Objects
  object3DFromMesh,
  object3DFromObjects,

-}
  
  -- * Misc Functions

  cameraLookAt,
  HGamer3D.Graphics3D.Internal.Base.setBackgroundColour,
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
--  import HGamer3D.Graphics3D.Internal.PlatonShapes
  




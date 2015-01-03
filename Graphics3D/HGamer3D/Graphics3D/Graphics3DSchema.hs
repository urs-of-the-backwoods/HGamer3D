{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
{-# OPTIONS_HADDOCK hide #-}
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

-- | Types which describe the Scene, the high level of a Scene Tree
module HGamer3D.Graphics3D.Graphics3DSchema

where

import Data.Typeable
import HGamer3D.Data as Dat



{- ----------------------------------------------------------------
   Camera
   ---------------------------------------------------------------- -}

-- | The data to specify a camera
data Camera = Camera { 
  frustum :: Frustum,     -- ^ Frustum of the camera ,
  viewport :: Viewport    -- ^ Where to draw on the window
                     } deriving (Eq, Show, Typeable)
                                
-- | The frustum defines the area from which objects are rendered into the camera
data Frustum = Frustum {
  nearDistance :: Float,    -- ^ clipping distance near, objects which are nearer are not shown
  farDistance :: Float,     -- ^ clipping distance far, objects, which are more away are not shown
  fieldOfViewHorizontal :: Dat.Angle  -- ^ The field of view angle horizontally, the vertical direction is automatically determined by the aspect ratio of the final viewport window.
  } deriving (Eq, Show, Typeable)

deriving instance Typeable1 Dat.Rectangle

data Viewport = Viewport {
  vpZOrder :: Int,              -- ^ Z order in case of overlapping Viewports, higher ZOrders are on top
  vpPosition :: Dat.Rectangle Float,   -- ^ position of viewport in Window, coordinates reaching from 0.0 to 1.0
  vpBackgroundColor :: Colour
} deriving (Eq, Show, Typeable)

{- ----------------------------------------------------------------
   Geometry
   ---------------------------------------------------------------- -}

data Geometry = 

  -- loaded from resources
  ResourceGeometry String

  -- Primitive 3D Geometries
  | Cube
  | Sphere
--  | Cylinder

  -- Platon Geometries
  | Ikosaeder
  | Dodekaeder

  -- 2D Geometries
  | Plane
--  | Line

  deriving (Eq, Show, Typeable)
  
{- ----------------------------------------------------------------
   Material
   ---------------------------------------------------------------- -}

-- | The material itself
data Material = ResourceMaterial String -- ^ the name given the material resource in the resource system
              deriving (Eq, Show, Typeable)

{- ----------------------------------------------------------------
   Figure
   ---------------------------------------------------------------- -}

data Figure = SimpleFigure Geometry Material
            | ResourceFigure String
            | CombinedFigure [(Dat.Position, Dat.Orientation, Dat.Size, Figure)]
           deriving (Eq, Show, Typeable)

{- ----------------------------------------------------------------
   Light
   ---------------------------------------------------------------- -}

-- | The Light data type


data Light = Light {
  diffuseColour :: Colour,
  specularColour :: Colour,
  lightType :: LightType
  } deriving (Eq, Show, Typeable)

data LightType = PointLight                  -- ^ A source shining from one location
               | DirectionalLight Vec3       -- ^ A source shining from one direction (like the sun)
               | SpotLight Vec3 Angle Angle  -- ^ A source shining from one location into one direction with a cone,
                                             --   angles are inner angle, outer angle (a flashlight).
                                             --   Angles should be between 5 and 355 degrees.
           deriving (Eq, Show, Typeable)
  
{- ----------------------------------------------------------------
   Scene
   ---------------------------------------------------------------- -}

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


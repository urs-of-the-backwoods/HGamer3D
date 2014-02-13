-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2014 Peter Althainz
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

-- Rectangle.hs

-- | Type definitions for 2D geometry and windows
module HGamer3D.Data.ScreenGeometry
(
  -- * Windowing
  Window (..),
  
  -- * Geometry
  Point (..),
  Rectangle (..),
  rectFromPoints,
  pointsFromRect
  
) where
  
-- | A window is simply represented by it's window id, an unsigned C int
data Window = Window Int

-- | A point has two coordinates an x and y one
data Point = Point {
  ptX::Int, 
  ptY::Int } deriving (Eq, Show)
                      
-- | A rectangle has an a position as x and y and widht and height
data Rectangle = Rectangle {
  rectX :: Int,
  rectY :: Int,
  rectWidth :: Int,
  rectHeight :: Int } deriving (Eq, Show)

-- | derive a rectangle from upper left and lower right points
rectFromPoints :: Point -> Point -> Rectangle
rectFromPoints upperLeft lowerRight = Rectangle x y w h where
  x = ptX upperLeft
  y = ptY upperLeft
  w = (ptX lowerRight) - x
  h = (ptY lowerRight) - y
  
-- | get upper left and lower right point from a rect
pointsFromRect :: Rectangle -> (Point, Point)
pointsFromRect rect = (ul, lr) where
  x = rectX rect
  y = rectY rect
  x' = x + (rectWidth rect)
  y' = y + (rectHeight rect)
  ul = Point x y
  lr = Point x' y'
  

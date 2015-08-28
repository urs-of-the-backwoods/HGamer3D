-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2014 - 2015 Peter Althainz
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

-- HGamer3D/Data/Geometry2D.hs

{-# LANGUAGE FlexibleInstances #-}

-- | Type definitions for 2D geometry 
module HGamer3D.Data.Geometry2D
(
  -- * Geometry
  Point (..),
  Rectangle (..),
  rectFromPoints,
  pointsFromRect
  
) where
  
import Data.MessagePack
import HGamer3D.Data.Component

-- | A point has two coordinates an x and y one
data Num a => Point a = Point {
  ptX :: a,
  ptY :: a
  } deriving (Eq, Show)

instance ComponentClass (Point Float) where
  toObj (Point a b) = ObjectArray [ObjectFloat a, ObjectFloat b]
  fromObj (ObjectArray [ObjectFloat a, ObjectFloat b]) = (Point a b)
                      
instance ComponentClass (Point Int) where
  toObj (Point a b) = ObjectArray [ObjectInt (fromIntegral a), ObjectInt (fromIntegral b)]
  fromObj (ObjectArray [ObjectInt a, ObjectInt b]) = (Point (fromIntegral a) (fromIntegral b))
                      
-- | A rectangle has an a position as x and y and widht and height
data Num a => Rectangle a = Rectangle {
  rectX :: a,
  rectY :: a,
  rectWidth :: a,
  rectHeight :: a } deriving (Eq, Show)

instance ComponentClass (Rectangle Float) where
  toObj (Rectangle x y w h) = ObjectArray [ObjectFloat x, ObjectFloat y, ObjectFloat w, ObjectFloat h]
  fromObj (ObjectArray [ObjectFloat x, ObjectFloat y, ObjectFloat w, ObjectFloat h]) = (Rectangle x y w h)

instance ComponentClass (Rectangle Int) where
  toObj (Rectangle x y w h) = ObjectArray [ObjectInt (fromIntegral x), ObjectInt (fromIntegral y), ObjectInt (fromIntegral w), ObjectInt (fromIntegral h)]
  fromObj (ObjectArray [ObjectInt x, ObjectInt y, ObjectInt w, ObjectInt h]) = Rectangle (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

-- | derive a rectangle from upper left and lower right points
rectFromPoints :: Num a => Point a -> Point a -> Rectangle a
rectFromPoints upperLeft lowerRight = Rectangle rx ry rw rh where
  rx = ptX upperLeft
  ry = ptY upperLeft
  rw = (ptY lowerRight) - rx
  rh = (ptY lowerRight) - ry
  
-- | get upper left and lower right point from a rect
pointsFromRect :: Num a => Rectangle a -> (Point a, Point a)
pointsFromRect rect = (ul, lr) where
  rx = rectX rect
  ry = rectY rect
  rx' = rx + (rectWidth rect)
  ry' = ry + (rectHeight rect)
  ul = Point rx ry
  lr = Point rx' ry'
  

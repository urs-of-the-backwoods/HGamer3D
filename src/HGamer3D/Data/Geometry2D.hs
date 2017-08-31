{-
	2D Geometry
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2011-2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Data/Geometry2D.hs
-}
  
{-# LANGUAGE FlexibleInstances #-}

-- | Type definitions for 2D geometry 
module HGamer3D.Data.Geometry2D
where
  
import HGamer3D.Data.ScreenRect
import Fresco
import Data.Int

-- | A point has two coordinates an x and y one
data Point = Point {
  ptX :: Int,
  ptY :: Int
  } deriving (Eq, Show)

-- | derive a rectangle from upper left and lower right points
rectFromPoints :: (Point, Point) -> ScreenRect
rectFromPoints (upperLeft, lowerRight) = ScreenRect rx ry rw rh where
  rx = ptX upperLeft
  ry = ptY upperLeft
  rw = (ptY lowerRight) - rx
  rh = (ptY lowerRight) - ry
  
-- | get upper left and lower right point from a rect
pointsFromRect :: ScreenRect -> (Point, Point)
pointsFromRect rect = (ul, lr) where
  rx = screenRectX rect
  ry = screenRectY rect
  rx' = rx + (screenRectWidth rect)
  ry' = ry + (screenRectHeight rect)
  ul = Point rx ry
  lr = Point rx' ry'
  

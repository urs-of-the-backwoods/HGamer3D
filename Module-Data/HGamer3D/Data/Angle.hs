-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.althainz.de/HGamer3D.html
--
-- (c) 2011 Peter Althainz
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

-- Angle.hs

{- |
	Angles as Degrees or Radians, based on Float datatype
-}

module HGamer3D.Data.Angle 

(
	-- export Radians and Degrees 
	Angle (..),
	fromAngle,
	addA,
	subA,
	mulA,
	divA,
	sinA,
	cosA,
	tanA,
	asinA,
	acosA,
	atanA,
	
	Radians (..),
	Degrees (..),
)

where


data Angle = Rad Float | Deg Float deriving (Eq, Ord, Show)

sinA :: Angle -> Float
sinA = sin . fromAngle

cosA :: Angle -> Float
cosA = cos . fromAngle

tanA :: Angle -> Float
tanA = tan . fromAngle

asinA :: Float -> Angle
asinA = Rad . asin

acosA :: Float -> Angle
acosA = Rad . acos

atanA :: Float -> Angle
atanA = Rad . atan

addA :: Angle -> Angle -> Angle
addA a b = Rad $ (fromAngle a) + (fromAngle b)

subA :: Angle -> Angle -> Angle
subA a b = Rad $ (fromAngle a) - (fromAngle b)

mulA :: Angle -> Float -> Angle
mulA a b = Rad $ (fromAngle a) * b

divA :: Angle -> Float -> Angle
divA a b = Rad $ (fromAngle a) / b

data Radians = Radians Float deriving (Eq, Ord, Show)
data Degrees = Degrees Float deriving (Eq, Ord, Show)

class Angles a where
	toAngle :: a -> Angle
	fromAngle :: Angle -> a
	
instance Angles Float where
	toAngle r = Rad r
	fromAngle (Rad r) = r
	fromAngle (Deg d) = d/180*pi
	
instance Angles Radians where
	toAngle (Radians r) = Rad r
	fromAngle (Rad r) = (Radians r)
	fromAngle (Deg d) = (Radians (d/180*pi))
	
instance Angles Degrees where
	toAngle (Degrees d) = Deg d
	fromAngle (Deg d) = (Degrees d)
	fromAngle (Rad r) = (Degrees (r/pi*180))
	

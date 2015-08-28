-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011 - 2015 Peter Althainz
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

-- HGamer3D/Data/Angle.hs

-- | Angles as Degrees or Radians, based on Float datatype
module HGamer3D.Data.Angle 

(
	-- * Data definitions and conversions
	Angle (..),
	asRad,
	asDeg,
        
        -- * Mathematical functions
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
)

where

import Data.MessagePack
import HGamer3D.Data.Component

-- | construct an Angle either by giving it in degrees or radians
data Angle = Rad Float | Deg Float deriving (Eq, Ord, Show)

-- | value of an Angle as radiant
asRad :: Angle -> Float
asRad (Rad f) = f
asRad (Deg d) = d/180*pi

-- | value of an Angle as degree
asDeg :: Angle -> Float
asDeg (Deg f) = f
asDeg (Rad d) = d/pi*180


sinA :: Angle -> Float
sinA = sin . asRad

cosA :: Angle -> Float
cosA = cos . asRad

tanA :: Angle -> Float
tanA = tan . asRad

asinA :: Float -> Angle
asinA = Rad . asin

acosA :: Float -> Angle
acosA = Rad . acos

atanA :: Float -> Angle
atanA = Rad . atan

addA :: Angle -> Angle -> Angle
addA a b = Rad $ (asRad a) + (asRad b)

subA :: Angle -> Angle -> Angle
subA a b = Rad $ (asRad a) - (asRad b)

mulA :: Angle -> Float -> Angle
mulA a b = Rad $ (asRad a) * b

divA :: Angle -> Float -> Angle
divA a b = Rad $ (asRad a) / b

instance ComponentClass Angle where
	toObj a = ObjectFloat (asRad a)
	fromObj (ObjectFloat f) = Rad f 
	
	

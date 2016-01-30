{-
	Datatypes to specify a geometric angle
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2011-2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Data/Angle.hs
-}

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
import Fresco

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
    toObj (Rad v1) = ObjectArray [ObjectInt 0, ObjectArray [ObjectFloat v1]]
    toObj (Deg v1) = ObjectArray [ObjectInt 1, ObjectArray [ObjectFloat v1]]
    fromObj (ObjectArray [ObjectInt 0, ObjectArray [ObjectFloat v1]]) = Rad v1
    fromObj (ObjectArray [ObjectInt 1, ObjectArray [ObjectFloat v1]]) = Deg v1


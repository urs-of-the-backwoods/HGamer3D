{-
	Datatypes to specify a geometric angle
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2011-2017 Peter Althainz
	
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

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data Angle = Rad Float
    | Deg Float
    deriving (Eq, Read, Show)

instance Serialise Angle where
    encode (Rad v1) = encodeListLen 2 <>  encode (0::Int) <> encode v1
    encode (Deg v1) = encodeListLen 2 <>  encode (1::Int) <> encode v1
    decode = do
        decodeListLen
        i <- decode :: Decoder Int
        case i of
            0 -> (Rad <$> decode)
            1 -> (Deg <$> decode)


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


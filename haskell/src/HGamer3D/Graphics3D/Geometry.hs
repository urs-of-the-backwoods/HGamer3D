-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011 - 2017 Peter Althainz
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

-- HGamer3D/Graphics3D/Geometry.hs

-- | Module providing the Geometry type
module HGamer3D.Graphics3D.Geometry

(
        Shape (..),
        Geometry (..),
        ctGeometry,
        ctGraphicsElement
)

where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data Shape = Sphere
    | Cube
    | Plane
    | Cylinder
    | Pyramid
    | Torus
    deriving (Eq, Read, Show)

data Geometry = ShapeGeometry Shape
    | ResourceGeometry Text
    deriving (Eq, Read, Show)

ctGeometry :: ComponentType Geometry
ctGeometry = ComponentType 0xee433d1a4b964591

ctGraphicsElement :: ComponentType ()
ctGraphicsElement = ComponentType 0x65114ba821671643

instance Serialise Shape where
    encode (Sphere) = encodeListLen 1 <>  encode (0::Int) 
    encode (Cube) = encodeListLen 1 <>  encode (1::Int) 
    encode (Plane) = encodeListLen 1 <>  encode (2::Int) 
    encode (Cylinder) = encodeListLen 1 <>  encode (3::Int) 
    encode (Pyramid) = encodeListLen 1 <>  encode (4::Int) 
    encode (Torus) = encodeListLen 1 <>  encode (5::Int) 
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure Sphere)
            1 -> (pure Cube)
            2 -> (pure Plane)
            3 -> (pure Cylinder)
            4 -> (pure Pyramid)
            5 -> (pure Torus)

instance Serialise Geometry where
    encode (ShapeGeometry v1) = encodeListLen 2 <>  encode (0::Int) <> encode v1
    encode (ResourceGeometry v1) = encodeListLen 2 <>  encode (1::Int) <> encode v1
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (ShapeGeometry <$> decode)
            1 -> (ResourceGeometry <$> decode)



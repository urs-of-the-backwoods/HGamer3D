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
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data Shape = Cube
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

instance Serialise Shape where
    encode (Cube) = encode (0::Int) 
    encode (Plane) = encode (1::Int) 
    encode (Cylinder) = encode (2::Int) 
    encode (Pyramid) = encode (3::Int) 
    encode (Torus) = encode (4::Int) 
    decode = do
        i <- decode :: Decoder Int
        case i of
            0 -> (pure Cube)
            1 -> (pure Plane)
            2 -> (pure Cylinder)
            3 -> (pure Pyramid)
            4 -> (pure Torus)

instance Serialise Geometry where
    encode (ShapeGeometry v1) = encode (0::Int) <> encode v1
    encode (ResourceGeometry v1) = encode (1::Int) <> encode v1
    decode = do
        i <- decode :: Decoder Int
        case i of
            0 -> (ShapeGeometry <$> decode)
            1 -> (ResourceGeometry <$> decode)

ctGraphicsElement :: ComponentType ()
ctGraphicsElement = ComponentType 0x65114ba821671643
-- end of website text


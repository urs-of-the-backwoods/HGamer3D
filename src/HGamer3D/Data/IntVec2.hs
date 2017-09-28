module HGamer3D.Data.IntVec2
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


-- | Integer Vector 2 dimensions, for example for screen positions
data IntVec2 = IntVec2 {
    intVec2X::Int,
    intVec2Y::Int
    } deriving (Eq, Read, Show)

-- | position in a 2D plane, for example the srcreen
type Position2D = IntVec2

-- | size in a 2D plane, for example the srcreen
type Size2D = IntVec2

ctPosition2D :: ComponentType Position2D
ctPosition2D = ComponentType 0x7995a794e698f4b

ctSize2D :: ComponentType Size2D
ctSize2D = ComponentType 0x8a73da7bdfbe4ccc

instance Serialise IntVec2 where
    encode (IntVec2 v1 v2) = encodeListLen 2 <> encode v1 <> encode v2
    decode = decodeListLenOf 2 >> IntVec2 <$> decode <*> decode



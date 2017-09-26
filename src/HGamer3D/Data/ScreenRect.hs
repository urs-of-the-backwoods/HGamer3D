module HGamer3D.Data.ScreenRect
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


-- | a rectangle on the screen, given by start, width, height
data ScreenRect = ScreenRect {
    screenRectX::Int,
    screenRectY::Int,
    screenRectWidth::Int,
    screenRectHeight::Int
    } deriving (Eq, Read, Show)

ctScreenRect :: ComponentType ScreenRect
ctScreenRect = ComponentType 0x16877957e32da6b1

-- | rectangle dimensions for left, top, right, bottom
data ScreenRect2 = ScreenRect2 {
    screenRect2Left::Int,
    screenRect2Top::Int,
    screenRect2Right::Int,
    screenRect2Bottom::Int
    } deriving (Eq, Read, Show)

ctScreenRect2 :: ComponentType ScreenRect2
ctScreenRect2 = ComponentType 0x5009dcc85ea5f959

instance Serialise ScreenRect where
    encode (ScreenRect v1 v2 v3 v4) = encodeListLen 4 <> encode v1 <> encode v2 <> encode v3 <> encode v4
    decode = decodeListLenOf 4 >> ScreenRect <$> decode <*> decode <*> decode <*> decode

instance Serialise ScreenRect2 where
    encode (ScreenRect2 v1 v2 v3 v4) = encodeListLen 4 <> encode v1 <> encode v2 <> encode v3 <> encode v4
    decode = decodeListLenOf 4 >> ScreenRect2 <$> decode <*> decode <*> decode <*> decode



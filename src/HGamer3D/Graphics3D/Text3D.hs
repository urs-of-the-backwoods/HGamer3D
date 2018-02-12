module HGamer3D.Graphics3D.Text3D where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data FaceCameraMode = FCNone
    | FCRotateXYZ
    | FCRotateY
    | FCLookatXYZ
    | FCLookatY
    | FCLookatMixed
    | FCDirection
    deriving (Eq, Read, Show)

data Text3D = Text3D {
    text3DFont::Text,
    text3DFontSize::Int,
    text3DFCMode::FaceCameraMode,
    text3DFixedScreenSize::Bool
    } deriving (Eq, Read, Show)

ctText3D :: ComponentType Text3D
ctText3D = ComponentType 0x620bb5dd7dfca052

instance Serialise FaceCameraMode where
    encode (FCNone) = encodeListLen 1 <>  encode (0::Int) 
    encode (FCRotateXYZ) = encodeListLen 1 <>  encode (1::Int) 
    encode (FCRotateY) = encodeListLen 1 <>  encode (2::Int) 
    encode (FCLookatXYZ) = encodeListLen 1 <>  encode (3::Int) 
    encode (FCLookatY) = encodeListLen 1 <>  encode (4::Int) 
    encode (FCLookatMixed) = encodeListLen 1 <>  encode (5::Int) 
    encode (FCDirection) = encodeListLen 1 <>  encode (6::Int) 
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure FCNone)
            1 -> (pure FCRotateXYZ)
            2 -> (pure FCRotateY)
            3 -> (pure FCLookatXYZ)
            4 -> (pure FCLookatY)
            5 -> (pure FCLookatMixed)
            6 -> (pure FCDirection)

instance Serialise Text3D where
    encode (Text3D v1 v2 v3 v4) = encodeListLen 4 <> encode v1 <> encode v2 <> encode v3 <> encode v4
    decode = decodeListLenOf 4 >> Text3D <$> decode <*> decode <*> decode <*> decode


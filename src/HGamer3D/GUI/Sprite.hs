module HGamer3D.GUI.Sprite where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data Sprite = Sprite {
    spriteResource::Text,
    spriteOpacity::Float
    } deriving (Eq, Read, Show)

ctSprite :: ComponentType Sprite
ctSprite = ComponentType 0x39b4f64b33f5cb41

instance Serialise Sprite where
    encode (Sprite v1 v2) = encodeListLen 2 <> encode v1 <> encode v2
    decode = decodeListLenOf 2 >> Sprite <$> decode <*> decode


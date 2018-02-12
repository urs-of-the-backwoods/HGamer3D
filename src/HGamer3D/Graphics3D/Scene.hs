module HGamer3D.Graphics3D.Scene where

import Fresco

import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data Scene = XmlScene Text
    | BinaryScene Text
    deriving (Eq, Read, Show)

ctScene :: ComponentType Scene
ctScene = ComponentType 0x829863cdd141007e

instance Serialise Scene where
    encode (XmlScene v1) = encodeListLen 2 <>  encode (0::Int) <> encode v1
    encode (BinaryScene v1) = encodeListLen 2 <>  encode (1::Int) <> encode v1
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (XmlScene <$> decode)
            1 -> (BinaryScene <$> decode)


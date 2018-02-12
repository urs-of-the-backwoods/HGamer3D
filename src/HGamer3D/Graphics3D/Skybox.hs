module HGamer3D.Graphics3D.Skybox where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data Skybox = SkyboxMaterial Text
    deriving (Eq, Read, Show)

ctSkybox :: ComponentType Skybox
ctSkybox = ComponentType 0x457ac00afe66a3a4

instance Serialise Skybox where
    encode (SkyboxMaterial v1) = encodeListLen 2 <>  encode (0::Int) <> encode v1
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (SkyboxMaterial <$> decode)


module HGamer3D.Graphics3D.Particles where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data Particles = ParticleEffectResource Text
    deriving (Eq, Read, Show)

ctParticles :: ComponentType Particles
ctParticles = ComponentType 0x5009dcc85ea5f959

instance Serialise Particles where
    encode (ParticleEffectResource v1) = encodeListLen 2 <>  encode (0::Int) <> encode v1
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (ParticleEffectResource <$> decode)


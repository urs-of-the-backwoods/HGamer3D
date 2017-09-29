module HGamer3D.GUI.BlendMode
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


-- | combining (blening) images together with various operations
data BlendMode = BMReplace
    | BMAdd
    | BMMultiply
    | BMAlpha
    | BMAddAlpha
    | BMPremulAlpha
    | BMInvDestAlpha
    | BMSubtract
    | BMSubtractAlpha
    deriving (Eq, Read, Show)

ctBlendMode :: ComponentType BlendMode
ctBlendMode = ComponentType 0xe010f1bdee3d43a9

instance Serialise BlendMode where
    encode (BMReplace) = encodeListLen 1 <>  encode (0::Int) 
    encode (BMAdd) = encodeListLen 1 <>  encode (1::Int) 
    encode (BMMultiply) = encodeListLen 1 <>  encode (2::Int) 
    encode (BMAlpha) = encodeListLen 1 <>  encode (3::Int) 
    encode (BMAddAlpha) = encodeListLen 1 <>  encode (4::Int) 
    encode (BMPremulAlpha) = encodeListLen 1 <>  encode (5::Int) 
    encode (BMInvDestAlpha) = encodeListLen 1 <>  encode (6::Int) 
    encode (BMSubtract) = encodeListLen 1 <>  encode (7::Int) 
    encode (BMSubtractAlpha) = encodeListLen 1 <>  encode (8::Int) 
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure BMReplace)
            1 -> (pure BMAdd)
            2 -> (pure BMMultiply)
            3 -> (pure BMAlpha)
            4 -> (pure BMAddAlpha)
            5 -> (pure BMPremulAlpha)
            6 -> (pure BMInvDestAlpha)
            7 -> (pure BMSubtract)
            8 -> (pure BMSubtractAlpha)



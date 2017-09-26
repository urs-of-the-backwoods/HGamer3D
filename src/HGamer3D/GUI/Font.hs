module HGamer3D.GUI.Font
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


-- | simple definition of a font as property 
-- for example for GUI text
data Font = Font {
    -- | font design as a name or the name of the resource
    fontTypeface::Text,
    fontSize::Int
    } deriving (Eq, Read, Show)

ctFont :: ComponentType Font
ctFont = ComponentType 0x457ac00afe66a3a4

instance Serialise Font where
    encode (Font v1 v2) = encodeListLen 2 <> encode v1 <> encode v2
    decode = decodeListLenOf 2 >> Font <$> decode <*> decode



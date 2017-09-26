module HGamer3D.GUI.MinSize
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


-- | minimum size of an element, for self-adjusting elements
data MinSize = MinSize {
    -- | min width of this element
    minSizeMinWidth::Int,
    -- | min height of this element
    minSizeMinHeight::Int
    } deriving (Eq, Read, Show)

ctMinSize :: ComponentType MinSize
ctMinSize = ComponentType 0x7534a286d000125c

instance Serialise MinSize where
    encode (MinSize v1 v2) = encodeListLen 2 <> encode v1 <> encode v2
    decode = decodeListLenOf 2 >> MinSize <$> decode <*> decode



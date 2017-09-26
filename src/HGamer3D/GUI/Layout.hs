module HGamer3D.GUI.Layout
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative

import HGamer3D.Data.ScreenRect


-- | direction in which sub elements are laid out
data LayoutMode = LMFree
    | LMHorizontal
    | LMVertical
    deriving (Eq, Read, Show)

-- | how elements are placed on the screen
data Layout = Layout {
    -- | direction of layout or free
    layoutMode::LayoutMode,
    -- | space between elements
    layoutSpacing::Int,
    -- | borders of element
    layoutBorders::ScreenRect2
    } deriving (Eq, Read, Show)

ctLayout :: ComponentType Layout
ctLayout = ComponentType 0x1c94738af20a0ac6

instance Serialise LayoutMode where
    encode (LMFree) = encodeListLen 1 <>  encode (0::Int) 
    encode (LMHorizontal) = encodeListLen 1 <>  encode (1::Int) 
    encode (LMVertical) = encodeListLen 1 <>  encode (2::Int) 
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure LMFree)
            1 -> (pure LMHorizontal)
            2 -> (pure LMVertical)

instance Serialise Layout where
    encode (Layout v1 v2 v3) = encodeListLen 3 <> encode v1 <> encode v2 <> encode v3
    decode = decodeListLenOf 3 >> Layout <$> decode <*> decode <*> decode



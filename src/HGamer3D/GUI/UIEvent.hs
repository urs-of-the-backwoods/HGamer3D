module HGamer3D.GUI.UIEvent
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative

import HGamer3D.Input.Mouse
import HGamer3D.Data.IntVec2

-- | position and element data, which is relevant during a UI event
data UIEventPosition = UIEventPosition {
    -- | position of mouse on the screen
    uIEventPositionMousePosition::Position2D,
    -- | position of element involved
    uIEventPositionElementPosition::Position2D,
    -- | name of element involved, if available, if not empty string
    uIEventPositionElementName::Text
    } deriving (Eq, Read, Show)

-- | a UI mouse click event
data UIClick = Down UIEventPosition MouseButtonData
    | Up UIEventPosition MouseButtonData
    deriving (Eq, Read, Show)

ctUIClick :: ComponentType UIClick
ctUIClick = ComponentType 0x7669c3c292bf265

-- | a UI mouse hover event
data UIHover = Hover UIEventPosition MouseButtonData
    deriving (Eq, Read, Show)

ctUIHover :: ComponentType UIHover
ctUIHover = ComponentType 0x7e6f5eb9ae275c30

-- | ui mouse drag event
data UIDrag = Begin UIEventPosition MouseButtonData
    | Move UIEventPosition MouseButtonData
    | End UIEventPosition MouseButtonData
    deriving (Eq, Read, Show)

ctUIDrag :: ComponentType UIDrag
ctUIDrag = ComponentType 0x239d21291230fb3a

instance Serialise UIEventPosition where
    encode (UIEventPosition v1 v2 v3) = encodeListLen 3 <> encode v1 <> encode v2 <> encode v3
    decode = decodeListLenOf 3 >> UIEventPosition <$> decode <*> decode <*> decode

instance Serialise UIClick where
    encode (Down v1 v2) = encodeListLen 3 <>  encode (0::Int) <> encode v1<> encode v2
    encode (Up v1 v2) = encodeListLen 3 <>  encode (1::Int) <> encode v1<> encode v2
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (Down <$> decode <*> decode)
            1 -> (Up <$> decode <*> decode)

instance Serialise UIHover where
    encode (Hover v1 v2) = encodeListLen 3 <>  encode (0::Int) <> encode v1<> encode v2
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (Hover <$> decode <*> decode)

instance Serialise UIDrag where
    encode (Begin v1 v2) = encodeListLen 3 <>  encode (0::Int) <> encode v1<> encode v2
    encode (Move v1 v2) = encodeListLen 3 <>  encode (1::Int) <> encode v1<> encode v2
    encode (End v1 v2) = encodeListLen 3 <>  encode (2::Int) <> encode v1<> encode v2
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (Begin <$> decode <*> decode)
            1 -> (Move <$> decode <*> decode)
            2 -> (End <$> decode <*> decode)



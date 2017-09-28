module HGamer3D.GUI.UIEvent
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative

import HGamer3D.Data.IntVec2
import HGamer3D.Input.Mouse


-- | a UI mouse click event, sent by UI Subsystem, not by a single element
data UIClickEvent = NoClick
    | SingleClick Text Position2D MouseButtonData
    | DoubleClick Text Position2D MouseButtonData
    | ClickEnd Text Position2D MouseButtonData
    deriving (Eq, Read, Show)

ctUIClickEvent :: ComponentType UIClickEvent
ctUIClickEvent = ComponentType 0x7669c3c292bf265

-- | a UI mouse hover event, sent by a single element
data UIHoverEvent = NoHover
    | HoverBegin Text Position2D Position2D
    | HoverEnd Text
    deriving (Eq, Read, Show)

ctUIHoverEvent :: ComponentType UIHoverEvent
ctUIHoverEvent = ComponentType 0x7e6f5eb9ae275c30

-- | ui mouse drag event
data UIDragEvent = NoDrag
    | DragBegin Text Position2D Position2D
    | DragMove Text Position2D Position2D Position2D
    | DragEnd Text Position2D Position2D
    | DragCancel Text Position2D Position2D
    deriving (Eq, Read, Show)

ctUIDragEvent :: ComponentType UIDragEvent
ctUIDragEvent = ComponentType 0x239d21291230fb3a

instance Serialise UIClickEvent where
    encode (NoClick) = encodeListLen 1 <>  encode (0::Int) 
    encode (SingleClick v1 v2 v3) = encodeListLen 4 <>  encode (1::Int) <> encode v1<> encode v2<> encode v3
    encode (DoubleClick v1 v2 v3) = encodeListLen 4 <>  encode (2::Int) <> encode v1<> encode v2<> encode v3
    encode (ClickEnd v1 v2 v3) = encodeListLen 4 <>  encode (3::Int) <> encode v1<> encode v2<> encode v3
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure NoClick)
            1 -> (SingleClick <$> decode <*> decode <*> decode)
            2 -> (DoubleClick <$> decode <*> decode <*> decode)
            3 -> (ClickEnd <$> decode <*> decode <*> decode)

instance Serialise UIHoverEvent where
    encode (NoHover) = encodeListLen 1 <>  encode (0::Int) 
    encode (HoverBegin v1 v2 v3) = encodeListLen 4 <>  encode (1::Int) <> encode v1<> encode v2<> encode v3
    encode (HoverEnd v1) = encodeListLen 2 <>  encode (2::Int) <> encode v1
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure NoHover)
            1 -> (HoverBegin <$> decode <*> decode <*> decode)
            2 -> (HoverEnd <$> decode)

instance Serialise UIDragEvent where
    encode (NoDrag) = encodeListLen 1 <>  encode (0::Int) 
    encode (DragBegin v1 v2 v3) = encodeListLen 4 <>  encode (1::Int) <> encode v1<> encode v2<> encode v3
    encode (DragMove v1 v2 v3 v4) = encodeListLen 5 <>  encode (2::Int) <> encode v1<> encode v2<> encode v3<> encode v4
    encode (DragEnd v1 v2 v3) = encodeListLen 4 <>  encode (3::Int) <> encode v1<> encode v2<> encode v3
    encode (DragCancel v1 v2 v3) = encodeListLen 4 <>  encode (4::Int) <> encode v1<> encode v2<> encode v3
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure NoDrag)
            1 -> (DragBegin <$> decode <*> decode <*> decode)
            2 -> (DragMove <$> decode <*> decode <*> decode <*> decode)
            3 -> (DragEnd <$> decode <*> decode <*> decode)
            4 -> (DragCancel <$> decode <*> decode <*> decode)



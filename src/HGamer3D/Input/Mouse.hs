module HGamer3D.Input.Mouse
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data MouseMode = Absolute
    | Relative
    | Wrap
    deriving (Eq, Read, Show)

data MouseConfig = MouseConfig {
    mouseConfigMode::MouseMode
    } deriving (Eq, Read, Show)

ctMouseConfig :: ComponentType MouseConfig
ctMouseConfig = ComponentType 0xa532f43b1c1c6bc7

data MouseButtonData = MouseButtonData {
    mouseButtonDataButton::Int,
    mouseButtonDataButtons::Int,
    mouseButtonDataQualifiers::Int
    } deriving (Eq, Read, Show)

data MouseMoveData = MouseMoveData {
    mouseMoveDataX::Int,
    mouseMoveDataY::Int,
    mouseMoveDataDx::Int,
    mouseMoveDataDy::Int,
    mouseMoveDataButtons::Int,
    mouseMoveDataQualifiers::Int
    } deriving (Eq, Read, Show)

data MouseWheelData = MouseWheelData {
    mouseWheelDataWheel::Int,
    mouseWheelDataButtons::Int,
    mouseWheelDataQualifiers::Int
    } deriving (Eq, Read, Show)

data MouseEvent = NoMouseEvent
    | MButtonUpEvent MouseButtonData
    | MButtonDownEvent MouseButtonData
    | MMoveEvent MouseMoveData
    | MWheelEvent MouseWheelData
    deriving (Eq, Read, Show)

ctMouseEvent :: ComponentType MouseEvent
ctMouseEvent = ComponentType 0x27eaf3fd46595d08

data MouseClickEvent = NoMouseClick
    | MouseDownClick MouseButtonData
    | MouseUpClick MouseButtonData
    deriving (Eq, Read, Show)

ctMouseClickEvent :: ComponentType MouseClickEvent
ctMouseClickEvent = ComponentType 0x5bd46a46b4ae5d38

data MouseMoveEvent = NoMouseMove
    | MouseMove MouseMoveData
    deriving (Eq, Read, Show)

ctMouseMoveEvent :: ComponentType MouseMoveEvent
ctMouseMoveEvent = ComponentType 0x7a409f478c6a34f5

data MouseWheelEvent = NoMouseWheel
    | MouseWheel MouseWheelData
    deriving (Eq, Read, Show)

ctMouseWheelEvent :: ComponentType MouseWheelEvent
ctMouseWheelEvent = ComponentType 0xa5d6c1c359e6d8ce

instance Serialise MouseMode where
    encode (Absolute) = encodeListLen 1 <>  encode (0::Int) 
    encode (Relative) = encodeListLen 1 <>  encode (1::Int) 
    encode (Wrap) = encodeListLen 1 <>  encode (2::Int) 
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure Absolute)
            1 -> (pure Relative)
            2 -> (pure Wrap)

instance Serialise MouseConfig where
    encode (MouseConfig v1) = encodeListLen 1 <> encode v1
    decode = decodeListLenOf 1 >> MouseConfig <$> decode

instance Serialise MouseButtonData where
    encode (MouseButtonData v1 v2 v3) = encodeListLen 3 <> encode v1 <> encode v2 <> encode v3
    decode = decodeListLenOf 3 >> MouseButtonData <$> decode <*> decode <*> decode

instance Serialise MouseMoveData where
    encode (MouseMoveData v1 v2 v3 v4 v5 v6) = encodeListLen 6 <> encode v1 <> encode v2 <> encode v3 <> encode v4 <> encode v5 <> encode v6
    decode = decodeListLenOf 6 >> MouseMoveData <$> decode <*> decode <*> decode <*> decode <*> decode <*> decode

instance Serialise MouseWheelData where
    encode (MouseWheelData v1 v2 v3) = encodeListLen 3 <> encode v1 <> encode v2 <> encode v3
    decode = decodeListLenOf 3 >> MouseWheelData <$> decode <*> decode <*> decode

instance Serialise MouseEvent where
    encode (NoMouseEvent) = encodeListLen 1 <>  encode (0::Int) 
    encode (MButtonUpEvent v1) = encodeListLen 2 <>  encode (1::Int) <> encode v1
    encode (MButtonDownEvent v1) = encodeListLen 2 <>  encode (2::Int) <> encode v1
    encode (MMoveEvent v1) = encodeListLen 2 <>  encode (3::Int) <> encode v1
    encode (MWheelEvent v1) = encodeListLen 2 <>  encode (4::Int) <> encode v1
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure NoMouseEvent)
            1 -> (MButtonUpEvent <$> decode)
            2 -> (MButtonDownEvent <$> decode)
            3 -> (MMoveEvent <$> decode)
            4 -> (MWheelEvent <$> decode)

instance Serialise MouseClickEvent where
    encode (NoMouseClick) = encodeListLen 1 <>  encode (0::Int) 
    encode (MouseDownClick v1) = encodeListLen 2 <>  encode (1::Int) <> encode v1
    encode (MouseUpClick v1) = encodeListLen 2 <>  encode (2::Int) <> encode v1
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure NoMouseClick)
            1 -> (MouseDownClick <$> decode)
            2 -> (MouseUpClick <$> decode)

instance Serialise MouseMoveEvent where
    encode (NoMouseMove) = encodeListLen 1 <>  encode (0::Int) 
    encode (MouseMove v1) = encodeListLen 2 <>  encode (1::Int) <> encode v1
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure NoMouseMove)
            1 -> (MouseMove <$> decode)

instance Serialise MouseWheelEvent where
    encode (NoMouseWheel) = encodeListLen 1 <>  encode (0::Int) 
    encode (MouseWheel v1) = encodeListLen 2 <>  encode (1::Int) <> encode v1
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure NoMouseWheel)
            1 -> (MouseWheel <$> decode)



module HGamer3D.GUI.Button
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


-- | Button, containing text and event bool (to be deprecated)
data Button = Button {
    buttonPressed::Bool,
    buttonLabel::Text
    } deriving (Eq, Read, Show)

ctButton :: ComponentType Button
ctButton = ComponentType 0x68a1857c27690b30

-- | Button event, signals press and release actions
data ButtonEvent = NoButtonEvent
    | Pressed
    | Released
    deriving (Eq, Read, Show)

ctButtonEvent :: ComponentType ButtonEvent
ctButtonEvent = ComponentType 0x3049202a50c414a7

-- | Standard UI Button with ordinary style
type BasicButton = Text

ctBasicButton :: ComponentType BasicButton
ctBasicButton = ComponentType 0x372110c1ae4548b8

-- | UI Button, based on an image, which can be clicked to signal button action
type ImageButton = Text

ctImageButton :: ComponentType ImageButton
ctImageButton = ComponentType 0x916f39acd0fc989e

instance Serialise Button where
    encode (Button v1 v2) = encodeListLen 2 <> encode v1 <> encode v2
    decode = decodeListLenOf 2 >> Button <$> decode <*> decode

instance Serialise ButtonEvent where
    encode (NoButtonEvent) = encodeListLen 1 <>  encode (0::Int) 
    encode (Pressed) = encodeListLen 1 <>  encode (1::Int) 
    encode (Released) = encodeListLen 1 <>  encode (2::Int) 
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure NoButtonEvent)
            1 -> (pure Pressed)
            2 -> (pure Released)



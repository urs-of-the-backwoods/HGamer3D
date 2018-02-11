module HGamer3D.Data.WindowEvent where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data ScreenModeEvent = ScreenModeEvent {
    screenModeEventWidth::Int,
    screenModeEventHeight::Int,
    screenModeEventFullscreen::Bool,
    screenModeEventBorderless::Bool
    } deriving (Eq, Read, Show)

ctScreenModeEvent :: ComponentType ScreenModeEvent
ctScreenModeEvent = ComponentType 0x7534a286d000125c

type ExitRequestedEvent = ()

ctExitRequestedEvent :: ComponentType ExitRequestedEvent
ctExitRequestedEvent = ComponentType 0xbd86f89ddca9280f

instance Serialise ScreenModeEvent where
    encode (ScreenModeEvent v1 v2 v3 v4) = encodeListLen 4 <> encode v1 <> encode v2 <> encode v3 <> encode v4
    decode = decodeListLenOf 4 >> ScreenModeEvent <$> decode <*> decode <*> decode <*> decode


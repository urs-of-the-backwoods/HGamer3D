module HGamer3D.Data.LogMessage where

import Fresco
import HGamer3D.Graphics3D.Graphics3DConfig
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data LogMessage = LogMessage {
    logMessageLevel::LogLevel,
    logMessageMessage::Text
    } deriving (Eq, Read, Show)

ctLogMessage :: ComponentType LogMessage
ctLogMessage = ComponentType 0x1c94738af20a0ac6

instance Serialise LogMessage where
    encode (LogMessage v1 v2) = encodeListLen 2 <> encode v1 <> encode v2
    decode = decodeListLenOf 2 >> LogMessage <$> decode <*> decode


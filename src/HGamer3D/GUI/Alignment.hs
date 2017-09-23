module HGamer3D.GUI.Alignment
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data HorizontalAlignment = HALeft
    | HACenter
    | HARight
    deriving (Eq, Read, Show)

data VerticalAlignment = VATop
    | VACenter
    | VABottom
    deriving (Eq, Read, Show)

data Alignment = Alignment {
    alignmentHorizontal::HorizontalAlignment,
    alignmentVertical::VerticalAlignment
    } deriving (Eq, Read, Show)

ctAlignment :: ComponentType Alignment
ctAlignment = ComponentType 0x1cdc5b0a65479346

instance Serialise HorizontalAlignment where
    encode (HALeft) = encodeListLen 1 <>  encode (0::Int) 
    encode (HACenter) = encodeListLen 1 <>  encode (1::Int) 
    encode (HARight) = encodeListLen 1 <>  encode (2::Int) 
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure HALeft)
            1 -> (pure HACenter)
            2 -> (pure HARight)

instance Serialise VerticalAlignment where
    encode (VATop) = encodeListLen 1 <>  encode (0::Int) 
    encode (VACenter) = encodeListLen 1 <>  encode (1::Int) 
    encode (VABottom) = encodeListLen 1 <>  encode (2::Int) 
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure VATop)
            1 -> (pure VACenter)
            2 -> (pure VABottom)

instance Serialise Alignment where
    encode (Alignment v1 v2) = encodeListLen 2 <> encode v1 <> encode v2
    decode = decodeListLenOf 2 >> Alignment <$> decode <*> decode



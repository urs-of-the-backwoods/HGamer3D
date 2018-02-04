module HGamer3D.Data.Label where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


type Label = Text

ctLabel :: ComponentType Label
ctLabel = ComponentType 0xf98939ae4b0d693a


module HGamer3D.Data.Name
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


-- | identifying something by name, an element, object or widget
type Name = Text

ctName :: ComponentType Name
ctName = ComponentType 0xf98939ae4b0d693a


